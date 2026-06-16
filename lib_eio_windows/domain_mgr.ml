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

open Eio.Std

[@@@alert "-unstable"]

module Fd = Eio_unix.Fd
module Trace = Eio.Private.Trace

let socketpair sched k ~sw ~domain ~ty ~protocol wrap_a wrap_b =
  let open Effect.Deep in
  match
    let unix_a, unix_b = Sched.socketpair domain ty protocol in
    (* Wrap (attaching the switch close-hook) before associating, so a failed
       association doesn't leak the raw sockets. *)
    let a = Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix:true unix_a in
    let b = Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix:true unix_b in
    Fd.use_exn "socketpair-a" a (Sched.associate sched);
    Fd.use_exn "socketpair-b" b (Sched.associate sched);
    (wrap_a a, wrap_b b)
  with
  | r -> continue k r
  | exception Unix.Unix_error (code, name, arg) ->
    discontinue k (Err.wrap code name arg)

(* Run an event loop in the current domain, using [fn x] as the root fiber. *)
let run_event_loop fn x =
  Sched.with_sched @@ fun sched ->
  let open Effect.Deep in
  let extra_effects : _ effect_handler = {
    effc = fun (type a) (e : a Effect.t) : ((a, Sched.exit) continuation -> Sched.exit) option ->
      match e with
      | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k Time.mono_clock)
      | Eio_unix.Net.Import_socket_stream (sw, close_unix, unix_fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix unix_fd in
          Fd.use_exn "import-stream" fd (Sched.associate sched);
          continue k (Flow.of_fd fd :> _ Eio_unix.Net.stream_socket)
        )
      | Eio_unix.Net.Import_socket_listening (sw, close_unix, unix_fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix unix_fd in
          Fd.use_exn "import-listening" fd (Sched.associate sched);
          continue k (Net.listening_socket ~hook:Switch.null_hook fd)
        )
      | Eio_unix.Net.Import_socket_datagram (sw, close_unix, unix_fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix unix_fd in
          Fd.use_exn "import-datagram" fd (Sched.associate sched);
          continue k (Net.datagram_socket fd)
        )
      | Eio_unix.Net.Socketpair_stream (sw, domain, protocol) -> Some (fun k ->
          let wrap fd = (Flow.of_fd fd :> _ Eio_unix.Net.stream_socket) in
          socketpair sched k ~sw ~domain ~protocol ~ty:Unix.SOCK_STREAM wrap wrap
        )
      | Eio_unix.Net.Socketpair_datagram (sw, domain, protocol) -> Some (fun k ->
          let wrap fd = Net.datagram_socket fd in
          socketpair sched k ~sw ~domain ~protocol ~ty:Unix.SOCK_DGRAM wrap wrap
        )
      | Eio_unix.Private.Pipe sw -> Some (fun k ->
          match
            let r, w = Low_level.pipe ~sw in
            (* Associate here (not in [Low_level.pipe]): we're inside an effect
               handler, so we use the in-scope [sched] rather than [Sched.get]. *)
            Fd.use_exn "pipe-r" r (Sched.associate sched);
            Fd.use_exn "pipe-w" w (Sched.associate sched);
            let source = Flow.of_pipe_source r in
            let sink = Flow.of_fd w in
            (source, sink)
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
            discontinue k (Err.wrap code name arg)
        )
      | _ -> None
  }
  in
  Sched.run ~extra_effects sched fn x

let wrap_backtrace fn x =
  match fn x with
  | x -> Ok x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Error (ex, bt)

let unwrap_backtrace = function
  | Ok x -> x
  | Error (ex, bt) -> Printexc.raise_with_backtrace ex bt

module Impl = struct
  type t = unit

  let domain_spawn ctx enqueue fn =
    Domain.spawn @@ fun () ->
    Trace.domain_spawn ~parent:(Eio.Private.Fiber_context.tid ctx);
    Fun.protect fn ~finally:(fun () -> enqueue (Ok ()))

  let run_raw () fn =
    let domain = ref None in
    Eio.Private.Suspend.enter "run-domain" (fun ctx enqueue ->
        domain := Some (domain_spawn ctx enqueue (wrap_backtrace fn))
      );
    Trace.with_span "Domain.join" @@ fun () ->
    unwrap_backtrace (Domain.join (Option.get !domain))

  let run () fn =
    let domain = ref None in
    Eio.Private.Suspend.enter "run-domain" (fun ctx enqueue ->
        let cancelled, set_cancelled = Promise.create () in
        Eio.Private.Fiber_context.set_cancel_fn ctx (Promise.resolve set_cancelled);
        domain := Some (domain_spawn ctx enqueue (fun () ->
            run_event_loop (wrap_backtrace (fun () -> fn ~cancelled)) ()
          ))
      );
    Trace.with_span "Domain.join" @@ fun () ->
    unwrap_backtrace (Domain.join (Option.get !domain))
end

let v =
  let handler = Eio.Domain_manager.Pi.mgr (module Impl) in
  Eio.Resource.T ((), handler)
