(*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
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

[@@@alert "-unstable"]

open Eio.Std

module Fiber_context = Eio.Private.Fiber_context
module Ctf = Eio.Private.Ctf
module Fd = Eio_unix.Fd

module Suspended = Eio_utils.Suspended
module Zzz = Eio_utils.Zzz
module Lf_queue = Eio_utils.Lf_queue

module Low_level = Low_level

(* When renaming, we get a plain [Eio.Fs.dir]. We need extra access to check
   that the new location is within its sandbox. *)
type ('t, _, _) Eio.Resource.pi += Dir_fd : ('t, 't -> Low_level.dir_fd, [> `Dir_fd]) Eio.Resource.pi

let get_dir_fd_opt (Eio.Resource.T (t, ops)) =
  match Eio.Resource.get_opt ops Dir_fd with
  | Some f -> Some (f t)
  | None -> None

(* When copying between a source with an FD and a sink with an FD, we can share the chunk
   and avoid copying. *)
let fast_copy src dst =
  let fallback () =
    (* No chunks available. Use regular memory instead. *)
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Low_level.readv src [buf] in
        Low_level.writev dst [Cstruct.sub buf 0 got]
      done
    with End_of_file -> ()
  in
  Low_level.with_chunk ~fallback @@ fun chunk ->
  let chunk_size = Uring.Region.length chunk in
  try
    while true do
      let got = Low_level.read_upto src chunk chunk_size in
      Low_level.write dst chunk got
    done
  with End_of_file -> ()

(* Try a fast copy using splice. If the FDs don't support that, switch to copying. *)
let _fast_copy_try_splice src dst =
  try
    while true do
      let _ : int = Low_level.splice src ~dst ~len:max_int in
      ()
    done
  with
  | End_of_file -> ()
  | Eio.Exn.Io (Eio.Exn.X Eio_unix.Unix_error ((EAGAIN | EINVAL), "splice", _), _) -> fast_copy src dst

(* XXX workaround for issue #319, PR #327 *)
let fast_copy_try_splice src dst = fast_copy src dst

(* Copy using the [Read_source_buffer] optimisation.
   Avoids a copy if the source already has the data. *)
let copy_with_rsb rsb dst =
  try
    while true do
      rsb (Low_level.writev_single dst)
    done
  with End_of_file -> ()

(* Copy by allocating a chunk from the pre-shared buffer and asking
   the source to write into it. This used when the other methods
   aren't available. *)
let fallback_copy (type src) (module Src : Eio.Flow.Pi.SOURCE with type t = src) src dst =
  let fallback () =
    (* No chunks available. Use regular memory instead. *)
    let buf = Cstruct.create 4096 in
    try
      while true do
        let got = Src.single_read src buf in
        Low_level.writev dst [Cstruct.sub buf 0 got]
      done
    with End_of_file -> ()
  in
  Low_level.with_chunk ~fallback @@ fun chunk ->
  let chunk_cs = Uring.Region.to_cstruct chunk in
  try
    while true do
      let got = Src.single_read src chunk_cs in
      Low_level.write dst chunk got
    done
  with End_of_file -> ()

module Datagram_socket = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let fd t = t

  let close = Eio_unix.Fd.close

  let send t ?dst buf =
    let dst = Option.map Eio_unix.Net.sockaddr_to_unix dst in
    let sent = Low_level.send_msg t ?dst buf in
    assert (sent = Cstruct.lenv buf)

  let recv t buf =
    let addr, recv = Low_level.recv_msg t [buf] in
    Eio_unix.Net.sockaddr_of_unix_datagram (Uring.Sockaddr.get addr), recv

  let shutdown t cmd =
    Low_level.shutdown t @@ match cmd with
    | `Receive -> Unix.SHUTDOWN_RECEIVE
    | `Send -> Unix.SHUTDOWN_SEND
    | `All -> Unix.SHUTDOWN_ALL
end

let datagram_handler = Eio_unix.Pi.datagram_handler (module Datagram_socket)

let datagram_socket fd =
  Eio.Resource.T (fd, datagram_handler)

module Flow = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let fd t = t

  let close = Eio_unix.Fd.close

  let is_tty t = Fd.use_exn "isatty" t Unix.isatty

  let stat = Low_level.fstat

  let single_read t buf =
    if is_tty t then (
      (* Work-around for https://github.com/axboe/liburing/issues/354
         (should be fixed in Linux 5.14) *)
      Low_level.await_readable t
    );
    Low_level.readv t [buf]

  let pread t ~file_offset bufs =
    Low_level.readv ~file_offset t bufs

  let pwrite t ~file_offset bufs =
    Low_level.writev_single ~file_offset t bufs

  let read_methods = []

  let single_write t bufs = Low_level.writev_single t bufs

  let copy t ~src =
    match Eio_unix.Resource.fd_opt src with
    | Some src -> fast_copy_try_splice src t
    | None ->
      let Eio.Resource.T (src, ops) = src in
      let module Src = (val (Eio.Resource.get ops Eio.Flow.Pi.Source)) in
      let rec aux = function
        | Eio.Flow.Read_source_buffer rsb :: _ -> copy_with_rsb (rsb src) t
        | _ :: xs -> aux xs
        | [] -> fallback_copy (module Src) src t
      in
      aux Src.read_methods

  let shutdown t cmd =
    Low_level.shutdown t @@ match cmd with
    | `Receive -> Unix.SHUTDOWN_RECEIVE
    | `Send -> Unix.SHUTDOWN_SEND
    | `All -> Unix.SHUTDOWN_ALL

  let send_msg t ~fds data =
    Low_level.send_msg t ~fds data

  let recv_msg_with_fds t ~sw ~max_fds data =
    let _addr, n, fds = Low_level.recv_msg_with_fds t ~sw ~max_fds data in
    n, fds
end

let flow_handler = Eio_unix.Pi.flow_handler (module Flow)

let flow fd =
  let r = Eio.Resource.T (fd, flow_handler) in
  (r : [`Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r :>
     [< `Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r)

let source fd = (flow fd :> _ Eio_unix.source)
let sink   fd = (flow fd :> _ Eio_unix.sink)

module Listening_socket = struct
  type t = Fd.t

  type tag = [`Generic | `Unix]

  let fd t = t

  let close = Fd.close

  let accept t ~sw =
    Switch.check sw;
    let client, client_addr = Low_level.accept ~sw t in
    let client_addr = match client_addr with
      | Unix.ADDR_UNIX path         -> `Unix path
      | Unix.ADDR_INET (host, port) -> `Tcp (Eio_unix.Net.Ipaddr.of_unix host, port)
    in
    let flow = (flow client :> _ Eio.Net.stream_socket) in
    flow, client_addr
end

let listening_handler = Eio_unix.Pi.listening_socket_handler (module Listening_socket)

let listening_socket fd =
  Eio.Resource.T (fd, listening_handler)

let socket_domain_of = function
  | `Unix _ -> Unix.PF_UNIX
  | `UdpV4 -> Unix.PF_INET
  | `UdpV6 -> Unix.PF_INET6
  | `Udp (host, _)
  | `Tcp (host, _) ->
    Eio.Net.Ipaddr.fold host
      ~v4:(fun _ -> Unix.PF_INET)
      ~v6:(fun _ -> Unix.PF_INET6)

let connect ~sw connect_addr =
  let addr = Eio_unix.Net.sockaddr_to_unix connect_addr in
  let sock_unix = Unix.socket ~cloexec:true (socket_domain_of connect_addr) Unix.SOCK_STREAM 0 in
  let sock = Fd.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
  Low_level.connect sock addr;
  (flow sock :> _ Eio_unix.Net.stream_socket)

module Impl = struct
  type t = unit
  type tag = [`Unix | `Generic]

  let listen () ~reuse_addr ~reuse_port ~backlog ~sw listen_addr =
    if reuse_addr then (
      match listen_addr with
      | `Tcp _ -> ()
      | `Unix path ->
        match Unix.lstat path with
        | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        | exception Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg
    );
    let addr = Eio_unix.Net.sockaddr_to_unix listen_addr in
    let sock_unix = Unix.socket ~cloexec:true (socket_domain_of listen_addr) Unix.SOCK_STREAM 0 in
    let sock = Fd.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
    (* For Unix domain sockets, remove the path when done (except for abstract sockets). *)
    begin match listen_addr with
      | `Unix path ->
        if String.length path > 0 && path.[0] <> Char.chr 0 then
          Switch.on_release sw (fun () -> Unix.unlink path)
      | `Tcp _ -> ()
    end;
    if reuse_addr then
      Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
    if reuse_port then
      Unix.setsockopt sock_unix Unix.SO_REUSEPORT true;
    Unix.bind sock_unix addr;
    Unix.listen sock_unix backlog;
    (listening_socket sock :> _ Eio.Net.listening_socket_ty r)

  let connect () ~sw addr = (connect ~sw addr :> [`Generic | `Unix] Eio.Net.stream_socket_ty r)

  let datagram_socket () ~reuse_addr ~reuse_port ~sw saddr =
    if reuse_addr then (
      match saddr with
      | `Udp _ | `UdpV4 | `UdpV6 -> ()
      | `Unix path ->
        match Unix.lstat path with
        | Unix.{ st_kind = S_SOCK; _ } -> Unix.unlink path
        | _ -> ()
        | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
        | exception Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg
    );
    let sock_unix = Unix.socket ~cloexec:true (socket_domain_of saddr) Unix.SOCK_DGRAM 0 in
    let sock = Fd.of_unix ~sw ~seekable:false ~close_unix:true sock_unix in
    begin match saddr with
    | `Udp _ | `Unix _ as saddr ->
      let addr = Eio_unix.Net.sockaddr_to_unix saddr in
      if reuse_addr then
        Unix.setsockopt sock_unix Unix.SO_REUSEADDR true;
      if reuse_port then
        Unix.setsockopt sock_unix Unix.SO_REUSEPORT true;
      Unix.bind sock_unix addr
    | `UdpV4 | `UdpV6 -> ()
    end;
    (datagram_socket sock :> [`Generic | `Unix] Eio.Net.datagram_socket_ty r)

  let getaddrinfo () = Low_level.getaddrinfo
  let getnameinfo () = Eio_unix.Net.getnameinfo
end

let net =
  let handler = Eio.Net.Pi.network (module Impl) in
  Eio.Resource.T ((), handler)

type stdenv = Eio_unix.Stdenv.base

module Process = Low_level.Process

let process proc : Eio.Process.t = object
  method pid = Process.pid proc

  method await =
    match Eio.Promise.await @@ Process.exit_status proc with
    | Unix.WEXITED i -> `Exited i
    | Unix.WSIGNALED i -> `Signaled i
    | Unix.WSTOPPED _ -> assert false

  method signal i = Process.signal proc i
end

(* fchdir wants just a directory FD, not an FD and a path like the *at functions. *)
let with_dir dir_fd path fn =
  Switch.run @@ fun sw ->
  Low_level.openat ~sw
    ~seekable:false
    ~access:`R
    ~perm:0
    ~flags:Uring.Open_flags.(cloexec + path + directory)
    dir_fd (if path = "" then "." else path)
  |> fn

let process_mgr = object
  inherit Eio_unix.Process.mgr

  method spawn_unix ~sw ?cwd ~env ~fds ~executable args =
    let actions = Process.Fork_action.[
        Eio_unix.Private.Fork_action.inherit_fds fds;
        execve executable ~argv:(Array.of_list args) ~env
    ] in
    let with_actions cwd fn = match cwd with
      | None -> fn actions
      | Some (fd, s) ->
        match get_dir_fd_opt fd with
        | None -> Fmt.invalid_arg "cwd is not an OS directory!"
        | Some dir_fd ->
          with_dir dir_fd s @@ fun cwd ->
          fn (Process.Fork_action.fchdir cwd :: actions)
    in
    with_actions cwd @@ fun actions ->
    process (Process.spawn ~sw actions)
end

let wrap_backtrace fn x =
  match fn x with
  | x -> Ok x
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Error (ex, bt)

let unwrap_backtrace = function
  | Ok x -> x
  | Error (ex, bt) -> Printexc.raise_with_backtrace ex bt

let domain_mgr ~run_event_loop = object
  inherit Eio.Domain_manager.t

  method run_raw fn =
    let domain = ref None in
    Sched.enter (fun t k ->
        domain := Some (Domain.spawn (fun () -> Fun.protect (wrap_backtrace fn) ~finally:(fun () -> Sched.enqueue_thread t k ())))
      );
    unwrap_backtrace (Domain.join (Option.get !domain))

  method run fn =
    let domain = ref None in
    Sched.enter (fun t k ->
        let cancelled, set_cancelled = Promise.create () in
        Fiber_context.set_cancel_fn k.fiber (Promise.resolve set_cancelled);
        domain := Some (Domain.spawn (fun () ->
            Fun.protect
              (fun () ->
                 let result = ref None in
                 let fn = wrap_backtrace (fun () -> fn ~cancelled) in
                 run_event_loop (fun () -> result := Some (fn ())) ();
                 Option.get !result
              )
              ~finally:(fun () -> Sched.enqueue_thread t k ())))
      );
    unwrap_backtrace (Domain.join (Option.get !domain))
end

module Mono_clock = struct
  type t = unit
  type time = Mtime.t

  let now () = Mtime_clock.now ()
  let sleep_until () time = Low_level.sleep_until time
end

let mono_clock : Mtime.t Eio.Time.clock_ty r =
  let handler = Eio.Time.Pi.clock (module Mono_clock) in
  Eio.Resource.T ((), handler)

module Clock = struct
  type t = unit
  type time = float

  let now () = Unix.gettimeofday ()

  let sleep_until () time =
    (* todo: use the realtime clock directly instead of converting to monotonic time.
       That is needed to handle adjustments to the system clock correctly. *)
    let d = time -. Unix.gettimeofday () in
    Eio.Time.Mono.sleep mono_clock d
end

let clock : float Eio.Time.clock_ty r =
  let handler = Eio.Time.Pi.clock (module Clock) in
  Eio.Resource.T ((), handler)

module rec Dir : sig
  include Eio.Fs.Pi.DIR

  val v : label:string -> path:string -> Low_level.dir_fd -> t

  val close : t -> unit

  val fd : t -> Low_level.dir_fd
end = struct
  type t = {
    fd : Low_level.dir_fd;
    label : string;
    path : string;
  }

  let v ~label ~path fd = { fd; label; path }

  let open_in t ~sw path =
    let fd = Low_level.openat ~sw t.fd path
        ~access:`R
        ~flags:Uring.Open_flags.cloexec
        ~perm:0
    in
    (flow fd :> Eio.File.ro_ty r)

  let open_out t ~sw ~append ~create path =
    let perm, flags =
      match create with
      | `Never            -> 0,    Uring.Open_flags.empty
      | `If_missing  perm -> perm, Uring.Open_flags.creat
      | `Or_truncate perm -> perm, Uring.Open_flags.(creat + trunc)
      | `Exclusive   perm -> perm, Uring.Open_flags.(creat + excl)
    in
    let flags = if append then Uring.Open_flags.(flags + append) else flags in
    let fd = Low_level.openat ~sw t.fd path
        ~access:`RW
        ~flags:Uring.Open_flags.(cloexec + flags)
        ~perm
    in
    (flow fd :> Eio.File.rw_ty r)

  let native_internal t path =
    if Filename.is_relative path then (
      let p = Filename.concat t.path path in
      if p = "" then "."
      else if p = "." then p
      else if Filename.is_implicit p then "./" ^ p
      else p
    ) else path

  let open_dir t ~sw path =
    let fd = Low_level.openat ~sw ~seekable:false t.fd (if path = "" then "." else path)
        ~access:`R
        ~flags:Uring.Open_flags.(cloexec + path + directory)
        ~perm:0
    in
    let label = Filename.basename path in
    let d = v ~label ~path:(native_internal t path) (Low_level.FD fd) in
    Eio.Resource.T (d, Dir_handler.v)

  let mkdir t ~perm path = Low_level.mkdir_beneath ~perm t.fd path

  let read_dir t path =
    Switch.run @@ fun sw ->
    let fd = Low_level.open_dir ~sw t.fd (if path = "" then "." else path) in
    Low_level.read_dir fd

  let close t =
    match t.fd with
    | FD x -> Fd.close x
    | Cwd | Fs -> failwith "Can't close non-FD directory!"

  let unlink t path = Low_level.unlink ~rmdir:false t.fd path
  let rmdir t path = Low_level.unlink ~rmdir:true t.fd path

  let rename t old_path t2 new_path =
    match get_dir_fd_opt t2 with
    | Some fd2 -> Low_level.rename t.fd old_path fd2 new_path
    | None -> raise (Unix.Unix_error (Unix.EXDEV, "rename-dst", new_path))

  let pp f t = Fmt.string f (String.escaped t.label)

  let fd t = t.fd

  let native t path =
    Some (native_internal t path)
end
and Dir_handler : sig
  val v : (Dir.t, [`Dir | `Close]) Eio.Resource.handler
end = struct
  let v = Eio.Resource.handler [
      H (Eio.Fs.Pi.Dir, (module Dir));
      H (Eio.Resource.Close, Dir.close);
      H (Dir_fd, Dir.fd);
    ]
end

let dir ~label ~path fd = Eio.Resource.T (Dir.v ~label ~path fd, Dir_handler.v)

module Secure_random = struct
  type t = unit
  let single_read () buf = Low_level.getrandom buf; Cstruct.length buf
  let read_methods = []
end

let secure_random =
  let ops = Eio.Flow.Pi.source (module Secure_random) in
  Eio.Resource.T ((), ops)

let stdenv ~run_event_loop =
  let stdin = source Eio_unix.Fd.stdin in
  let stdout = sink Eio_unix.Fd.stdout in
  let stderr = sink Eio_unix.Fd.stderr in
  let fs = (dir ~label:"fs" ~path:"" Fs, "") in
  let cwd = (dir ~label:"cwd" ~path:"" Cwd, "") in
  object (_ : stdenv)
    method stdin  = stdin
    method stdout = stdout
    method stderr = stderr
    method net = net
    method process_mgr = process_mgr
    method domain_mgr = domain_mgr ~run_event_loop
    method clock = clock
    method mono_clock = mono_clock
    method fs = (fs :> Eio.Fs.dir_ty Eio.Path.t)
    method cwd = (cwd :> Eio.Fs.dir_ty Eio.Path.t)
    method secure_random = secure_random
    method debug = Eio.Private.Debug.v
    method backend_id = "linux"
  end

let run_event_loop (type a) ?fallback config (main : _ -> a) arg : a =
  Sched.with_sched ?fallback config @@ fun st ->
  let open Effect.Deep in
  let extra_effects : _ effect_handler = {
    effc = fun (type a) (e : a Effect.t) : ((a, Sched.exit) continuation -> Sched.exit) option ->
      match e with
      | Eio_unix.Private.Get_monotonic_clock -> Some (fun k -> continue k mono_clock)
      | Eio_unix.Net.Import_socket_stream (sw, close_unix, fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~seekable:false ~close_unix fd in
          continue k (flow fd :> _ Eio_unix.Net.stream_socket)
        )
      | Eio_unix.Net.Import_socket_datagram (sw, close_unix, fd) -> Some (fun k ->
          let fd = Fd.of_unix ~sw ~seekable:false ~close_unix fd in
          continue k (datagram_socket fd)
        )
      | Eio_unix.Net.Socketpair_stream (sw, domain, protocol) -> Some (fun k ->
          match
            let a, b = Unix.socketpair ~cloexec:true domain Unix.SOCK_STREAM protocol in
            let a = Fd.of_unix ~sw ~seekable:false ~close_unix:true a |> flow in
            let b = Fd.of_unix ~sw ~seekable:false ~close_unix:true b |> flow in
            ((a :> _ Eio_unix.Net.stream_socket), (b :> _ Eio_unix.Net.stream_socket))
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
              discontinue k (Err.wrap code name arg)
        )
      | Eio_unix.Net.Socketpair_datagram (sw, domain, protocol) -> Some (fun k ->
          match
            let a, b = Unix.socketpair ~cloexec:true domain Unix.SOCK_DGRAM protocol in
            let a = Fd.of_unix ~sw ~seekable:false ~close_unix:true a |> datagram_socket in
            let b = Fd.of_unix ~sw ~seekable:false ~close_unix:true b |> datagram_socket in
            ((a :> _ Eio_unix.Net.datagram_socket), (b :> _ Eio_unix.Net.datagram_socket))
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
              discontinue k (Err.wrap code name arg)
        )
      | Eio_unix.Private.Pipe sw -> Some (fun k ->
          match
            let r, w = Low_level.pipe ~sw in
            let r = (flow r :> _ Eio_unix.source) in
            let w = (flow w :> _ Eio_unix.sink) in
            (r, w)
          with
          | r -> continue k r
          | exception Unix.Unix_error (code, name, arg) ->
            discontinue k (Err.wrap code name arg)
        )
      | _ -> None
  } in
  Sched.run ~extra_effects st main arg

let run ?queue_depth ?n_blocks ?block_size ?polling_timeout ?fallback main =
  let config = Sched.config ?queue_depth ?n_blocks ?block_size ?polling_timeout () in
  let stdenv = stdenv ~run_event_loop:(run_event_loop ?fallback:None config) in
  (* SIGPIPE makes no sense in a modern application. *)
  Sys.(set_signal sigpipe Signal_ignore);
  run_event_loop ?fallback config main stdenv
