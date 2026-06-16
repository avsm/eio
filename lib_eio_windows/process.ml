open Eio.Std

module Fd = Eio_unix.Fd

(* [eio_spawn cwd env exe stdin stdout stderr cmdline] starts a child process.
   [cwd] and [exe] are "" when not given, [env] empty to inherit the parent's.
   Returns [(pid, process_handle)]. *)
external eio_spawn :
  string -> string array -> string ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  string -> int * Unix.file_descr
  = "caml_eio_windows_spawn_bytes" "caml_eio_windows_spawn"

(* Like [eio_spawn], but attaches the child to a pseudoconsole instead of wiring
   explicit stdio handles. [cwd]/[exe] are "" when not given, [env] empty to
   inherit. *)
(* Exactly five arguments, so a single stub name (no [value* argv] bytecode
   wrapper): with two names bytecode would call the first with the wrong calling
   convention. *)
external eio_spawn_pty :
  string -> string array -> string -> Pty.conpty -> string -> int * Unix.file_descr
  = "caml_eio_windows_spawn_pty"

external eio_process_wait : Unix.file_descr -> int = "caml_eio_windows_process_wait"
external eio_process_terminate : Unix.file_descr -> int -> unit = "caml_eio_windows_process_terminate"

(* Quote arguments following the [CommandLineToArgvW] rules, since [CreateProcessW]
   takes a single command line rather than an argument vector. *)
let needs_quoting s =
  s = "" || String.exists (function ' ' | '\t' | '\n' | '\011' | '"' -> true | _ -> false) s

let quote_arg arg =
  if not (needs_quoting arg) then arg
  else begin
    let b = Buffer.create (String.length arg + 2) in
    Buffer.add_char b '"';
    let n = String.length arg in
    let i = ref 0 in
    while !i < n do
      let bs = ref 0 in
      while !i < n && arg.[!i] = '\\' do incr bs; incr i done;
      if !i = n then
        (* Escape all trailing backslashes so they don't escape the final quote. *)
        Buffer.add_string b (String.make (2 * !bs) '\\')
      else if arg.[!i] = '"' then begin
        Buffer.add_string b (String.make (2 * !bs + 1) '\\');
        Buffer.add_char b '"';
        incr i
      end else begin
        Buffer.add_string b (String.make !bs '\\');
        Buffer.add_char b arg.[!i];
        incr i
      end
    done;
    Buffer.add_char b '"';
    Buffer.contents b
  end

let command_line args = String.concat " " (List.map quote_arg args)

module Process = struct
  type t = {
    pid : int;
    handle : Fd.t;
    mutable status : int option;
    lock : Eio.Mutex.t;           (* Serialises the wait and [status] cache. *)
  }

  let pid t = t.pid

  (* Terminated processes report this exit code (the [uExitCode] passed to
     [TerminateProcess]); Windows has no signals, so a killed child is
     indistinguishable from one that exited with this code of its own accord. *)
  let terminate_exit_code = 1

  (* Wait (in a worker thread) for the process to exit, caching the result.
     Serialise the wait and cache under [t.lock]; make the wait itself
     non-cancellable (the interim systhread wait can't be interrupted). There is
     no kill-on-cancel: cancelling [await] does not terminate the child, it
     blocks until the child exits on its own.

     If [t.status] is already cached (e.g. reaped by the switch-release [stop]
     hook), it is returned without touching the fd, so [await] is safe even after
     the handle has been closed. The fd is only used when no status is cached,
     which after a spawn only happens while the handle is still open; should it
     ever be reached with a closed handle, raise a clear error rather than the
     opaque [Fd.use_exn] "used after close". *)
  let exit_code t =
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.lock t.lock;
    Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.lock) @@ fun () ->
    match t.status with
    | Some c -> c
    | None ->
      let c =
        Eio_unix.run_in_systhread ~label:"process_wait" (fun () ->
            Fd.use t.handle eio_process_wait
              ~if_closed:(fun () ->
                  failwith "process_wait: handle closed before the process was reaped"))
      in
      t.status <- Some c;
      c

  let await t : Eio.Process.exit_status =
    (* Windows has no signals, so even a terminated process just reports an exit code. *)
    `Exited (exit_code t)

  let signal t (_signum : int) =
    (* Windows has no POSIX signals; the best we can do is terminate. A closed
       handle / already-exited process is ignored, matching {!Eio.Process.signal}. *)
    Fd.use t.handle ~if_closed:ignore (fun h -> eio_process_terminate h terminate_exit_code)

  (* Switch-release hook (see [spawn_unix]): terminate the child, then reap and
     cache its exit status while the handle is still open, so a later [await]
     reads the cache instead of the closed fd. Order matters: [signal] first so
     that any [await] already blocked in the systhread wait (holding [t.lock])
     unblocks and caches the status; only then does [exit_code] take the lock —
     which now either succeeds immediately or returns the just-cached value —
     avoiding a deadlock against that in-flight wait. The reap is short because
     the process has just been terminated. *)
  let stop t =
    signal t Sys.sigkill;
    ignore (exit_code t : int)
end

module Process_impl = struct
  type t = Process.t
  type tag = [ `Generic | `Unix ]

  let pid = Process.pid
  let await = Process.await
  let signal = Process.signal
end

let process_handler = Eio.Process.Pi.process (module Process_impl)
let process t = Eio.Resource.T (t, process_handler)

(* Flow/FD plumbing, copied from the internal (unexported) {!Eio_unix.Process} helpers. *)

let with_close_list fn =
  let to_close = ref [] in
  let close () = List.iter Fd.close !to_close in
  match fn to_close with
  | x -> close (); x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    close ();
    Printexc.raise_with_backtrace ex bt

let read_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some f ->
    match Eio_unix.Resource.fd_opt f with
    | Some fd -> fd
    | None ->
      let r, w = Eio_unix.pipe sw in
      Fiber.fork ~sw (fun () -> Eio.Flow.copy f w; Eio.Flow.close w);
      let r = Eio_unix.Resource.fd r in
      to_close := r :: !to_close;
      r

let write_of_fd ~sw ~default ~to_close = function
  | None -> default
  | Some f ->
    match Eio_unix.Resource.fd_opt f with
    | Some fd -> fd
    | None ->
      let r, w = Eio_unix.pipe sw in
      Fiber.fork ~sw (fun () -> Eio.Flow.copy r f; Eio.Flow.close r);
      let w = Eio_unix.Resource.fd w in
      to_close := w :: !to_close;
      w

module Mgr = struct
  type t = unit
  type tag = [ `Generic | `Unix ]

  let pipe () ~sw =
    (Eio_unix.pipe sw :> ([Eio.Resource.close_ty | Eio.Flow.source_ty] r *
                          [Eio.Resource.close_ty | Eio.Flow.sink_ty] r))

  let spawn_unix () ~sw ?cwd ?pgid ?uid ?gid ?login_tty ~env ~fds ~executable args =
    (* Windows has no per-fd inheritance table, so only the three standard streams
       can be wired up; pgid/uid/gid have no Windows equivalent. *)
    if pgid <> None || uid <> None || gid <> None then
      Fmt.invalid_arg "spawn: pgid/uid/gid are not supported on Windows";
    (* [login_tty] is only meaningful when it is the tty token of a pty opened by
       this backend (see {!Pty}); found → attach that pseudoconsole. An arbitrary
       fd cannot be a controlling terminal on Windows, so it is rejected. *)
    let pty =
      match login_tty with
      | None -> None
      | Some fd ->
        match Pty.lookup fd with
        | Some pty -> Some pty
        | None -> Fmt.invalid_arg "spawn: login_tty is not an eio_windows pty"
    in
    (* Only the three standard streams can be wired up on Windows; reject any
       other descriptor rather than silently dropping it. When a pseudoconsole is
       attached the child's stdio comes from the console, so an explicit stdio
       mapping cannot be combined with it. *)
    (match pty with
     | Some _ when fds <> [] ->
       Fmt.invalid_arg "spawn: ~fds cannot be combined with a pty login_tty on Windows"
     | _ ->
       List.iter (fun (i, _, _) ->
           if i > 2 then Fmt.invalid_arg "spawn: only fds 0-2 are supported on Windows (got fd %d)" i)
         fds);
    (* Resolve the cwd through the directory capability's own sandbox check
       (rejecting [..] / absolute / outside-sandbox), then strip the NT prefix to a
       plain Win32 path for [CreateProcessW]. For a sandboxed capability this also
       yields an absolute path, decoupling the child's cwd from the launcher's; the
       unrestricted [fs] capability returns the path unchanged, so a relative cwd
       there is still resolved by [CreateProcessW] against the launcher's directory. *)
    let cwd =
      match cwd with
      | None -> ""
      | Some ((dir, p) : Eio.Fs.dir_ty Eio.Path.t) ->
        (* Resolve through the directory capability's own sandbox check (as the
           file ops and eio_posix do), so a confined cwd cannot escape via [..]
           or an absolute path. *)
        begin match Fs.Handler.as_posix_dir dir with
          | None -> Fmt.invalid_arg "cwd is not an eio_windows directory!"
          | Some d -> Fs.Dir.strip_nt_prefix (Err.run (Fs.Dir.resolve d) p)
        end
    in
    let cmdline = command_line args in
    let pid, raw_handle =
      match pty with
      | Some pty ->
        (* The pseudoconsole supplies the child's stdio; no explicit handles. *)
        eio_spawn_pty cwd env executable (Pty.hpcon pty) cmdline
      | None ->
        let find n =
          List.find_map (fun (i, fd, _) -> if i = n then Some fd else None) fds
        in
        let get n name =
          match find n with
          | Some fd -> fd
          | None -> Fmt.invalid_arg "spawn: no file descriptor for %s (fd %d)" name n
        in
        let stdin_fd = get 0 "stdin" and stdout_fd = get 1 "stdout" and stderr_fd = get 2 "stderr" in
        Fd.use_exn "stdin" stdin_fd @@ fun h0 ->
        Fd.use_exn "stdout" stdout_fd @@ fun h1 ->
        Fd.use_exn "stderr" stderr_fd @@ fun h2 ->
        eio_spawn cwd env executable h0 h1 h2 cmdline
    in
    let handle = Fd.of_unix ~sw ~blocking:true ~close_unix:true raw_handle in
    let t = { Process.pid; handle; status = None; lock = Eio.Mutex.create () } in
    (* Registered after [Fd.of_unix] so, LIFO, this runs before the handle is
       closed on switch release: it terminates the child and reaps its exit
       status into [t.status] while the handle is still open, so a later [await]
       observes the exit instead of raising on the closed fd. *)
    Switch.on_release sw (fun () -> Process.stop t);
    process t

  let spawn () ~sw ?cwd ?stdin ?stdout ?stderr ?env ?executable args =
    (* Match the cross-platform contract: an empty argument list is only valid if
       an explicit executable was given. *)
    if args = [] && executable = None then
      invalid_arg "Arguments list is empty and no executable given!";
    let env = match env with Some e -> e | None -> Unix.environment () in
    (* "" lets [CreateProcessW] find the program on $PATH (with $PATHEXT), shell-style. *)
    let executable = match executable with Some e -> e | None -> "" in
    with_close_list @@ fun to_close ->
    let stdin_fd  = read_of_fd  ~sw stdin  ~default:Fd.stdin  ~to_close in
    let stdout_fd = write_of_fd ~sw stdout ~default:Fd.stdout ~to_close in
    let stderr_fd = write_of_fd ~sw stderr ~default:Fd.stderr ~to_close in
    let fds = [
      0, stdin_fd, `Blocking;
      1, stdout_fd, `Blocking;
      2, stderr_fd, `Blocking;
    ] in
    spawn_unix () ~sw ?cwd ~env ~fds ~executable args
end

let mgr : Eio_unix.Process.mgr_ty r =
  let h = Eio_unix.Process.Pi.mgr_unix (module Mgr) in
  Eio.Resource.T ((), h)
