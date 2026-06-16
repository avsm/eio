(*
 * Copyright (C) 2023 Thomas Leonard
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* The Windows scheduler is a completion reactor built on an I/O completion port
   ([Iocp.t], one per domain). Overlapped operations are started and the fiber
   suspends on the operation's id; the scheduler blocks in [Iocp.wait] and
   resumes the matching fiber when its completion packet arrives. This replaces
   the old [Unix.select] readiness loop (which only worked on sockets and stalled
   the domain on blocking handles). Operations with no overlapped form (directory
   listing, [stat], [rename], process waits, ...) still run on the systhread pool,
   whose results wake the loop via [Iocp.wakeup]. *)

module Suspended = Eio_utils.Suspended
module Zzz = Eio_utils.Zzz
module Lf_queue = Eio_utils.Lf_queue
module Fiber_context = Eio.Private.Fiber_context
module Trace = Eio.Private.Trace

type exit = [`Exit_scheduler]

(* The type of items in the run queue. *)
type runnable =
  | IO : runnable                                       (* Reminder to check for IO *)
  | Thread : 'a Suspended.t * 'a -> runnable            (* Resume a fiber with a result value *)
  | Failed_thread : 'a Suspended.t * exn -> runnable    (* Resume a fiber with an exception *)

type t = {
  (* The queue of runnable fibers ready to be resumed. Note: other domains can also add work items here. *)
  run_q : runnable Lf_queue.t;

  (* This domain's completion port. All submission, reaping and cancellation
     happen on this domain; only [wakeup] (and the systhread pool's enqueues,
     which call it) touch it from other threads. *)
  iocp : Iocp.t;

  (* Fibers suspended on an in-flight overlapped operation, keyed by its id. The
     closure resumes the fiber with the completion. *)
  io_pending : (Iocp.completion_status -> unit) Iocp.H.t;

  (* Operations that couldn't be submitted because the OVERLAPPED pool was full.
     Retried (oldest first) whenever a slot frees up; each thunk returns [true]
     once it has taken a slot (or the pool is full again), [false] if it resolved
     without consuming one (see {!submit_io}). *)
  io_q : (unit -> bool) Queue.t;

  (* Ids of in-flight overlapped READ operations, keyed by the raw handle they
     were issued on. A receive-side [shutdown] cancels exactly these (see
     {!cancel_reads}) to wake a blocked reader, without aborting a concurrent
     send/writev on the same handle. An entry is added when a read takes an
     OVERLAPPED slot and removed when its completion is reaped. *)
  reads : (Unix.file_descr, Iocp.Id.t list ref) Hashtbl.t;

  mutable active_ops : int;             (* Exit when this is zero and [run_q] and [sleep_q] are empty. *)

  (* If [false], the main thread will check [run_q] before sleeping again
     (possibly because a wakeup has been or will be posted to the port).
     It can therefore be set to [false] in either of these cases:
     - By the receiving thread because it will check [run_q] before sleeping, or
     - By the sending thread because it will signal the main thread later *)
  need_wakeup : bool Atomic.t;

  sleep_q: Zzz.t;                       (* Fibers waiting for timers. *)

  thread_pool : Eio_unix.Private.Thread_pool.t;
}

(* This can be called from any systhread (including ones not running Eio),
   and also from signal handlers or GC finalizers. It must not take any locks.
   [PostQueuedCompletionStatus] is thread-safe. *)
let wakeup t =
  Atomic.set t.need_wakeup false; (* [t] will check [run_q] after getting the event below *)
  Iocp.wakeup t.iocp

(* Safe to call from anywhere (other systhreads, domains, signal handlers, GC finalizers) *)
let enqueue_thread t k x =
  Lf_queue.push t.run_q (Thread (k, x));
  if Atomic.get t.need_wakeup then wakeup t

(* Safe to call from anywhere (other systhreads, domains, signal handlers, GC finalizers) *)
let enqueue_failed_thread t k ex =
  Lf_queue.push t.run_q (Failed_thread (k, ex));
  if Atomic.get t.need_wakeup then wakeup t

(* Can only be called from our own domain, so no need to check for wakeup. *)
let enqueue_at_head t k =
  Lf_queue.push_head t.run_q (Thread (k, ()))

let iocp t = t.iocp

(* Register [fd] with this domain's completion port so overlapped operations on
   it deliver completions here. Idempotent: re-associating an already-registered
   handle reports [EINVAL], which we ignore. *)
let associate t fd =
  try ignore (Iocp.handle_of_fd t.iocp fd 0 : Iocp.fd)
  with Unix.Unix_error (EINVAL, _, _) -> ()

(* Track in-flight overlapped reads per raw handle so a receive-side [shutdown]
   can cancel exactly the blocked reads on that handle (see {!cancel_reads}),
   leaving any concurrent send/writev on it untouched. *)
let register_read t fd id =
  match Hashtbl.find_opt t.reads fd with
  | Some ids -> ids := id :: !ids
  | None -> Hashtbl.add t.reads fd (ref [id])

let unregister_read t fd id =
  match Hashtbl.find_opt t.reads fd with
  | None -> ()
  | Some ids ->
    ids := List.filter (fun i -> not (Iocp.Id.equal i id)) !ids;
    if !ids = [] then Hashtbl.remove t.reads fd

(* Cancel every in-flight read on [fd]. Each cancelled recv still produces an
   [ERROR_OPERATION_ABORTED] completion, which the read path turns into
   end-of-file; cancelling an id whose completion is already in hand is a no-op. *)
let cancel_reads t fd =
  match Hashtbl.find_opt t.reads fd with
  | None -> ()
  | Some ids -> List.iter (fun id -> Iocp.cancel t.iocp id) !ids

(* A request parked because the OVERLAPPED pool was full. [dead] is set by the
   fiber's cancel_fn so the queue drain skips this entry: a fiber cancelled while
   parked is resumed at once rather than waiting for a slot to free, and the guard
   stops the later drain resuming it a second time. *)
type parked = { mutable dead : bool }

(* Start an overlapped operation and resume [k] when it completes.
   [submit ()] starts the op and returns its id, or [None] if the OVERLAPPED
   pool is exhausted (we then park the request and retry when a slot frees).
   [read] is the raw handle when the op is a recv, so it can be tracked for
   {!cancel_reads}.
   A synchronous failure (the stub raising before the op is queued) and a
   cancellation are both reported to [k]; otherwise [on_ok] resumes it with the
   completion.
   Returns [true] if a free OVERLAPPED slot was taken (the op is now in flight) or
   none was available (the pool was full and the op re-parked), and [false] if [k]
   was resolved without consuming a slot (it was cancelled, or failed
   synchronously). {!submit_pending} uses this to keep offering a freed slot to
   parked ops until one actually takes it. *)
let rec submit_io : type a. t -> a Suspended.t -> ?read:Unix.file_descr -> (unit -> Iocp.Id.t option) -> on_ok:(Iocp.completion_status -> unit) -> bool =
  fun t k ?read submit ~on_ok ->
  match Fiber_context.get_error k.fiber with
  | Some ex -> enqueue_failed_thread t k ex; false
  | None ->
    match submit () with
    | exception ex -> enqueue_failed_thread t k ex; false
    | None ->
      (* No slot free: park the request and retry when one frees. There is no id
         to cancel yet, so register a cancel_fn that resolves [k] immediately and
         marks the entry dead; the retry then skips a dead entry (returning [false]
         so the drain moves on) rather than resuming [k] again. *)
      let entry = { dead = false } in
      Fiber_context.set_cancel_fn k.fiber (fun ex ->
          entry.dead <- true;
          enqueue_failed_thread t k ex);
      Queue.push (fun () ->
          if entry.dead then false
          else (Fiber_context.clear_cancel_fn k.fiber;
                submit_io t k ?read submit ~on_ok)) t.io_q;
      true
    | Some id ->
      t.active_ops <- t.active_ops + 1;
      Option.iter (fun fd -> register_read t fd id) read;
      Iocp.H.replace t.io_pending id (fun cs ->
          Option.iter (fun fd -> unregister_read t fd id) read;
          Fiber_context.clear_cancel_fn k.fiber;
          match Fiber_context.get_error k.fiber with
          | Some e -> enqueue_failed_thread t k e   (* cancelled: report that, not the abort *)
          | None -> on_ok cs);
      Fiber_context.set_cancel_fn k.fiber (fun _ -> Iocp.cancel t.iocp id);
      true

(* A completion just freed an OVERLAPPED slot: hand it to the oldest parked op.
   If that op resolves without taking the slot (cancelled, or a synchronous
   failure), keep offering it to the next parked op until one takes it (or the
   pool fills again) or the queue drains — otherwise a parked op could be stranded
   with a free slot and no further completion to trigger another drain. *)
and submit_pending t =
  match Queue.take_opt t.io_q with
  | None -> ()
  | Some fn -> if not (fn ()) then submit_pending t

(* Reap one completion: resume the waiting fiber and free its slot. *)
let complete_io t (cs : Iocp.completion_status) =
  match Iocp.H.find_opt t.io_pending cs.id with
  | Some resume -> Iocp.H.remove t.io_pending cs.id; t.active_ops <- t.active_ops - 1; resume cs
  | None -> ()
  (* No waiter for this id: every in-flight op has exactly one [io_pending] entry,
     added in [submit_io] and removed only here (a cancelled op keeps its entry
     and is reaped through the [Some] branch, which reports the cancellation). Not
     expected in normal operation; tolerated defensively against a stray packet. *)

(* Switch control to the next ready continuation.
   If none is ready, wait until we get a completion (or wakeup) and then switch.
   Returns only if there is nothing to do and no active operations. *)
let rec next t : [`Exit_scheduler] =
  match Lf_queue.pop t.run_q with
  | None -> assert false    (* We should always have an IO job, at least *)
  | Some Thread (k, v) ->   (* We already have a runnable task *)
    Fiber_context.clear_cancel_fn k.fiber;
    Suspended.continue k v
  | Some Failed_thread (k, ex) ->
    Fiber_context.clear_cancel_fn k.fiber;
    Suspended.discontinue k ex
  | Some IO -> (* Note: be sure to re-inject the IO task before continuing! *)
    (* This is not a fair scheduler: timers always run before all other IO *)
    let now = Mtime_clock.now () in
    match Zzz.pop ~now t.sleep_q with
    | `Due k ->
      (* A sleeping task is now due *)
      Lf_queue.push t.run_q IO;                 (* Re-inject IO job in the run queue *)
      begin match k with
        | Fiber k -> Suspended.continue k ()
        | Fn fn -> fn (); next t
      end
    | `Wait_until _ | `Nothing as next_due ->
      let timeout =
        match next_due with
        | `Nothing -> (-1)              (* [Iocp.wait] treats -1 as INFINITE *)
        | `Wait_until time ->
          let time = Mtime.to_uint64_ns time in
          let now = Mtime.to_uint64_ns now in
          let diff_ns = Int64.sub time now in
          if Int64.compare diff_ns 0L <= 0 then 0
          else
            (* Round up nanoseconds to whole milliseconds so we never wake early,
               and clamp below [INFINITE]: a multi-week timeout could otherwise
               truncate to 0xFFFFFFFF when narrowed to the DWORD [Iocp.wait] takes
               and be misread as "wait forever". *)
            let ms = Int64.div (Int64.add diff_ns 999_999L) 1_000_000L in
            if Int64.compare ms 0x7fff_ffffL > 0 then 0x7fff_ffff else Int64.to_int ms
      in
      if timeout < 0 && t.active_ops = 0 && Lf_queue.is_empty t.run_q then (
        (* Nothing further can happen at this point. [submit_pending] offers each
           freed slot to parked ops until [io_q] drains, so with no op in flight
           [io_q] is necessarily empty. *)
        assert (Queue.is_empty t.io_q);
        Lf_queue.close t.run_q;      (* Just to catch bugs if something tries to enqueue later *)
        `Exit_scheduler
      ) else (
        Atomic.set t.need_wakeup true;
        let timeout =
          if Lf_queue.is_empty t.run_q then timeout
          else (
            (* Either we're just checking for IO to avoid starvation, or someone
               added a new job while we were setting [need_wakeup] to [true].
               They might or might not have seen that, so we can't be sure
               they'll send an event. *)
            0
          )
        in
        (* At this point we're not going to check [run_q] again before sleeping.
           If [need_wakeup] is still [true], this is fine because we don't promise to do that.
           If [need_wakeup = false], a wake-up event will arrive and wake us up soon. *)
        Trace.suspend_domain Begin;
        let packet = Iocp.wait t.iocp ~timeout in
        Trace.suspend_domain End;
        Atomic.set t.need_wakeup false;
        Lf_queue.push t.run_q IO;                   (* Re-inject IO job in the run queue *)
        (match packet with
         | None -> ()                               (* Timed out: a timer is now due. *)
         | Some (Iocp.Posted _) -> ()               (* A wakeup (or stray packet): re-check [run_q]. *)
         | Some (Iocp.Io cs) ->
           complete_io t cs;
           (* Only reaping a completion frees an OVERLAPPED, so this is the one
              place a parked op can now be submitted; retrying only here also
              preserves [io_q]'s oldest-first order. *)
           submit_pending t);
        next t
      )

(* Emulate readiness with a zero-byte overlapped recv/send: it completes once
   [fd] is readable/writable. [associate] runs inside the submission thunk so an
   association failure is reported to [k] like any other submission error rather
   than escaping the scheduler; it is idempotent, so a retry re-running it is
   harmless. (These probes only make sense on sockets.) *)
let await_ready t (k : unit Suspended.t) fd op =
  ignore (submit_io t k
    (fun () -> associate t fd; op t.iocp (Iocp.Handle.of_fd fd) [Cstruct.empty])
    ~on_ok:(fun _ -> enqueue_thread t k ()) : bool);
  next t

let await_readable t k fd = await_ready t k fd Iocp.recv
let await_writable t k fd = await_ready t k fd Iocp.send

(* Windows has no socketpair(); OCaml's AF_UNIX emulation binds the listener to
   an address that isn't unique across processes, so concurrent processes
   collide with EADDRINUSE/EACCES. Emulate it ourselves, binding to a
   per-process-unique path (PID + counter), retrying only if a stale file from a
   crashed process happens to collide. *)
let socketpair_id = Atomic.make 0

let rec unix_socketpair tries =
  let path =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "eio-%d-%d.sock"
         (Unix.getpid ()) (Atomic.fetch_and_add socketpair_id 1))
  in
  let listener = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let cleanup () =
    Unix.close listener;
    try Unix.unlink path with Unix.Unix_error _ -> ()
  in
  match
    Unix.bind listener (Unix.ADDR_UNIX path);
    Unix.listen listener 1;
    let a = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    (match Unix.connect a (Unix.ADDR_UNIX path) with
     | () -> let b, _ = Unix.accept ~cloexec:true listener in (a, b)
     | exception e -> Unix.close a; raise e)
  with
  | pair -> cleanup (); pair
  | exception Unix.Unix_error ((EADDRINUSE | EACCES), _, _) when tries > 0 ->
    cleanup (); unix_socketpair (tries - 1)
  | exception e -> cleanup (); raise e

(* AF_UNIX stream pairs use our unique-address emulation; anything else (rare on
   Windows) falls back to the stdlib. *)
let socketpair domain ty protocol =
  match domain, ty with
  | Unix.PF_UNIX, Unix.SOCK_STREAM -> unix_socketpair 100
  | _ -> Unix.socketpair ~cloexec:true domain ty protocol

let with_sched fn =
  let run_q = Lf_queue.create () in
  Lf_queue.push run_q IO;
  let sleep_q = Zzz.create () in
  let iocp = Iocp.create 1 in
  let io_pending = Iocp.H.create 64 in
  let io_q = Queue.create () in
  let reads = Hashtbl.create 16 in
  let thread_pool = Eio_unix.Private.Thread_pool.create ~sleep_q in
  let t = { run_q; iocp; io_pending; io_q; reads;
            active_ops = 0; need_wakeup = Atomic.make false; sleep_q; thread_pool } in
  fn t

let get_enqueue t k = function
  | Ok v -> enqueue_thread t k v
  | Error ex -> enqueue_failed_thread t k ex

let await_timeout t (k : unit Suspended.t) time =
  match Fiber_context.get_error k.fiber with
  | Some e -> Suspended.discontinue k e
  | None ->
    let node = Zzz.add t.sleep_q time (Fiber k) in
    Fiber_context.set_cancel_fn k.fiber (fun ex ->
        Zzz.remove t.sleep_q node;
        enqueue_failed_thread t k ex
      );
    next t

let with_op t fn x =
  t.active_ops <- t.active_ops + 1;
  match fn x with
  | r ->
    t.active_ops <- t.active_ops - 1;
    r
  | exception ex ->
    t.active_ops <- t.active_ops - 1;
    raise ex

[@@@alert "-unstable"]

type _ Effect.t += Enter : (t -> 'a Eio_utils.Suspended.t -> [`Exit_scheduler]) -> 'a Effect.t
let enter op fn = Trace.suspend_fiber op; Effect.perform (Enter fn)

type _ Effect.t += Get : t Effect.t
let get () = Effect.perform Get

(* Submit an overlapped operation built by [submit iocp] and suspend until it
   completes, returning the completion status. [op] labels it for tracing. *)
let enter_io ?read op submit =
  enter op (fun t k ->
      ignore (submit_io t k ?read (fun () -> submit t.iocp) ~on_ok:(fun cs -> enqueue_thread t k cs) : bool);
      next t)

let run ~extra_effects t main x =
  let rec fork ~new_fiber:fiber fn =
    let open Effect.Deep in
    Trace.fiber (Fiber_context.tid fiber);
    match_with fn ()
      { retc = (fun () -> Fiber_context.destroy fiber; next t);
        exnc = (fun ex ->
            Fiber_context.destroy fiber;
            Printexc.raise_with_backtrace ex (Printexc.get_raw_backtrace ())
          );
        effc = fun (type a) (e : a Effect.t) : ((a, [`Exit_scheduler]) Effect.Deep.continuation -> [`Exit_scheduler]) option ->
          match e with
          | Get -> Some (fun k -> Effect.Deep.continue k t)
          | Enter fn -> Some (fun k ->
              match Fiber_context.get_error fiber with
              | Some e -> discontinue k e
              | None -> fn t { Suspended.k; fiber }
            )
          | Eio.Private.Effects.Get_context -> Some (fun k -> continue k fiber)
          | Eio.Private.Effects.Suspend f -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              let enqueue = get_enqueue t k in
              f fiber enqueue;
              next t
            )
          | Eio.Private.Effects.Fork (new_fiber, f) -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              enqueue_at_head t k;
              fork ~new_fiber f
            )
          | Eio_unix.Private.Await_readable fd -> Some (fun k ->
              await_readable t { Suspended.k; fiber } fd
            )
          | Eio_unix.Private.Await_writable fd -> Some (fun k ->
              await_writable t { Suspended.k; fiber } fd
            )
          | Eio_unix.Private.Thread_pool.Run_in_systhread fn -> Some (fun k ->
              let k = { Suspended.k; fiber } in
              let enqueue x = enqueue_thread t k (x, t.thread_pool) in
              Eio_unix.Private.Thread_pool.submit t.thread_pool ~ctx:fiber ~enqueue fn;
              next t
            )
          | e -> extra_effects.Effect.Deep.effc e
      }
  in
  let result = ref None in
  let `Exit_scheduler =
    let new_fiber = Fiber_context.make_root () in
    Domain_local_await.using
      ~prepare_for_await:Eio_utils.Dla.prepare_for_await
      ~while_running:(fun () ->
          fork ~new_fiber (fun () ->
              Eio_unix.Private.Thread_pool.run t.thread_pool @@ fun () ->
              result := Some (with_op t main x);
            )
        )
  in
  match !result with
  | Some x -> x
  | None -> failwith "BUG in scheduler: deadlock detected"
