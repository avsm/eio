open Eio.Std

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
  Low_level.Fixed.use ~fallback @@ fun chunk ->
  let chunk_size = Low_level.Fixed.length chunk in
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
    
let[@tail_mod_cons] rec list_take n = function
  | [] -> []
  | x :: xs ->
    if n = 0 then []
    else x :: list_take (n - 1) xs

let truncate_to_iomax xs =
  if List.compare_length_with xs Uring.iov_max <= 0 then xs
  else list_take Uring.iov_max xs

(* Copy using the [Read_source_buffer] optimisation.
   Avoids a copy if the source already has the data. *)
let copy_with_rsb rsb dst =
  let write xs = Low_level.writev_single dst (truncate_to_iomax xs) in
  try
    while true do rsb write done
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
  Low_level.Fixed.use ~fallback @@ fun chunk ->
  let chunk_cs = Low_level.Fixed.to_cstruct chunk in
  try
    while true do
      let got = Src.single_read src chunk_cs in
      Low_level.write dst chunk got
    done
  with End_of_file -> ()

module Impl = struct
  type tag = [`Generic | `Unix]

  type t = Eio_unix.Fd.t

  let fd t = t

  let close = Eio_unix.Fd.close

  let stat = Low_level.fstat

  let single_read t buf =
    Low_level.readv t [buf]

  let pread t ~file_offset bufs =
    Low_level.readv ~file_offset t bufs

  let pwrite t ~file_offset bufs =
    Low_level.writev_single ~file_offset t (truncate_to_iomax bufs)

  let read_methods = []

  let single_write t bufs = Low_level.writev_single t (truncate_to_iomax bufs)

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

  let seek = Low_level.lseek
  let sync = Low_level.fsync
  let truncate = Low_level.ftruncate

  let setsockopt = Low_level.setsockopt
  let getsockopt = Low_level.getsockopt
end

(* Each worker owns one buffer and submits its next operation as soon as its
   previous one completes. The scheduler can submit several workers' SQEs together. *)
let copy_ranges ~size_hint src dst =
  let module I = Optint.Int63 in
  (* Limit scratch space to 512 KiB per copy, even on very deep rings. *)
  let max_workers = min 4 (Uring.queue_depth (Sched.get ()).uring) in
  let chunk = min (I.of_int (128 * 1024)) size_hint in
  let last_chunk = I.div (I.pred size_hint) chunk in
  let workers = 1 + I.to_int (min (I.of_int (max_workers - 1)) last_chunk) in
  let next = ref I.zero in
  let eof = ref false in
  let worker () =
    let buf = Cstruct.create (I.to_int chunk) in
    let rec copy_chunk file_offset buf =
      match Low_level.readv ~file_offset src [buf] with
      | exception End_of_file -> eof := true
      | n ->
        Low_level.writev ~file_offset dst [Cstruct.sub buf 0 n];
        if n < Cstruct.length buf then
          copy_chunk (I.add file_offset (I.of_int n)) (Cstruct.shift buf n)
    in
    (* Claim offsets before yielding. Even after another worker observes EOF,
       finish every range already claimed, including any short reads. *)
    while not !eof do
      let offset = !next in
      next := I.add offset chunk;
      copy_chunk offset buf
    done
  in
  Switch.run @@ fun sw ->
  for _ = 2 to workers do Fiber.fork ~sw worker done;
  worker ()

(* Direct FD reads must return the same bytes as the source's read operations. *)
type 'a Eio.Flow.read_method += Copy_to_file of ('a -> Eio_unix.Fd.t -> unit)

module File = struct
  include Impl

  let copy_to_file src dst =
    if Eio_unix.Fd.is_seekable src then (
      match Low_level.fstat src with
      | { kind = `Regular_file; size; _ } when Optint.Int63.(compare size zero) > 0 ->
        copy_ranges ~size_hint:size src dst
      | { kind = `Block_device; _ } ->
        (* Linux reports st_size = 0 for block devices. Read until EOF. *)
        copy_ranges ~size_hint:Optint.Int63.max_int src dst
      | _ -> fast_copy src dst
    ) else fast_copy src dst

  let read_methods = [Copy_to_file copy_to_file]
end

let copy_file dst ~src:(Eio.Resource.T (t, ops) as src) =
  let module Source = (val Eio.Resource.get ops Eio.Flow.Pi.Source) in
  match Source.read_methods with
  | Copy_to_file copy :: _ -> copy t dst
  | _ -> Eio.Flow.Pi.simple_copy ~single_write:Impl.single_write dst ~src

let file_handler : (Eio_unix.Fd.t, [`Unix_fd | `Copy_file | Eio.File.rw_ty]) Eio.Resource.handler =
  Eio.Resource.handler (
    Eio.Resource.bindings (Eio_unix.Pi.file_handler (module File)) @
    [H (Eio.File.Pi.Copy_file, copy_file)]
  )

let of_fd fd =
  let r = Eio.Resource.T (fd, file_handler) in
  (r : [`Unix_fd | `Copy_file | Eio.File.rw_ty] r :>
     [< `Unix_fd | Eio.File.rw_ty] r)

let stream_handler = Eio_unix.Pi.stream_handler (module Impl)

let stream fd =
  let r = Eio.Resource.T (fd, stream_handler) in
  (r : [`Unix_fd | Eio_unix.Net.stream_socket_ty] r :>
     [< `Unix_fd | Eio_unix.Net.stream_socket_ty] r)

let source fd = (of_fd fd :> Eio_unix.source_ty r)
let sink   fd = (of_fd fd :> Eio_unix.sink_ty r)

let stdin = source Eio_unix.Fd.stdin
let stdout = sink Eio_unix.Fd.stdout
let stderr = sink Eio_unix.Fd.stderr

module Secure_random = struct
  type t = unit
  let single_read () buf = Low_level.getrandom buf; Cstruct.length buf
  let read_methods = []
end

let secure_random =
  let ops = Eio.Flow.Pi.source (module Secure_random) in
  Eio.Resource.T ((), ops)
