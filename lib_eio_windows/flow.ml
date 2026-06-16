open Eio.Std

module Impl = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let stat t =
    try Low_level.fstat t
    with Unix.Unix_error (code, name, arg) -> raise @@ Err.wrap code name arg

  let write_all t bufs =
    try Low_level.writev t bufs
    with Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)

  (* todo: provide a way to do a single write *)
  let single_write t bufs =
    write_all t bufs;
    Cstruct.lenv bufs

  (* Copy using the [Read_source_buffer] optimisation, avoiding a copy when the
     source already has the data in hand. *)
  let copy_with_rsb rsb dst =
    try while true do rsb (single_write dst) done
    with End_of_file -> ()

  let copy t ~src =
    let Eio.Resource.T (src_t, ops) = src in
    let module Src = (val (Eio.Resource.get ops Eio.Flow.Pi.Source)) in
    let rec aux = function
      | Eio.Flow.Read_source_buffer rsb :: _ -> copy_with_rsb (rsb src_t) t
      | _ :: xs -> aux xs
      | [] -> Eio.Flow.Pi.simple_copy ~single_write t ~src
    in
    aux Src.read_methods

  let single_read t buf =
    match Low_level.read_cstruct t buf with
    | 0 -> raise End_of_file
    | got -> got
    | exception (Unix.Unix_error (code, name, arg)) -> raise (Err.wrap code name arg)

  let shutdown t cmd =
    try
      Low_level.shutdown t @@ match cmd with
      | `Receive -> Unix.SHUTDOWN_RECEIVE
      | `Send -> Unix.SHUTDOWN_SEND
      | `All -> Unix.SHUTDOWN_ALL
    with
    | Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
    | Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)

  let read_methods = []

  let pread t ~file_offset bufs =
    let got = Low_level.preadv ~file_offset t (Array.of_list bufs) in
    if got = 0 then raise End_of_file
    else got

  let pwrite t ~file_offset bufs = Low_level.pwritev ~file_offset t (Array.of_list bufs)

  (* FD passing (SCM_RIGHTS) has no Windows equivalent; report it as a typed,
     catchable [Eio.Io] error rather than a bare [Failure]. *)
  let send_msg _t ~fds:_ _data = raise (Err.wrap Unix.EOPNOTSUPP "send_msg" "")

  let recv_msg_with_fds _t ~sw:_ ~max_fds:_ _data = raise (Err.wrap Unix.EOPNOTSUPP "recv_msg" "")

  let seek = Low_level.lseek
  let sync = Low_level.fsync
  let truncate = Low_level.ftruncate

  let fd t = t

  let close = Eio_unix.Fd.close

  let setsockopt t opt v = Err.run (Eio_unix.Net.setsockopt t opt) v
  let getsockopt t opt = Err.run (Eio_unix.Net.getsockopt t) opt
end

let handler = Eio_unix.Pi.flow_handler (module Impl)

let of_fd fd =
  let r = Eio.Resource.T (fd, handler) in
  (r : [`Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r :>
     [< `Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r)

(* Pipes are emulated with sockets (see {!Low_level.pipe}), so a peer that exits
   can surface as [ECONNRESET]/[EPIPE]/[ECONNABORTED] rather than a clean
   end-of-file. On the read side those just mean the data has ended, so treat them
   as end-of-file (writes still surface the reset as an error, as on POSIX).
   ([Low_level.normalise_error] already folds ERROR_NETNAME_DELETED /
   ERROR_CONNECTION_ABORTED to [ECONNRESET]; the [ECONNABORTED] case here is a raw
   WSAECONNABORTED completion.) *)
module Pipe_impl = struct
  include Impl

  let single_read t buf =
    match Low_level.read_cstruct t buf with
    | 0 -> raise End_of_file
    | got -> got
    | exception Unix.Unix_error ((ECONNRESET | EPIPE | ECONNABORTED), _, _) -> raise End_of_file
    | exception Unix.Unix_error (code, name, arg) -> raise (Err.wrap code name arg)
end

let pipe_handler = Eio_unix.Pi.flow_handler (module Pipe_impl)

let of_pipe_source fd =
  let r = Eio.Resource.T (fd, pipe_handler) in
  (r : [`Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r :>
     [< `Unix_fd | Eio_unix.Net.stream_socket_ty | Eio.File.rw_ty] r)

module Secure_random = struct
  type t = unit

  let single_read () buf =
    Low_level.getrandom buf;
    Cstruct.length buf

  let read_methods = []
end

let secure_random =
  let ops = Eio.Flow.Pi.source (module Secure_random) in
  Eio.Resource.T ((), ops)
