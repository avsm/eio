(** Adapt file handles to overlapped I/O.

    Sequential operations serialize access to each descriptor's file position.
    Positioned operations leave it unchanged and reopen synchronous handles by
    handle when necessary. *)

type kind = Synchronous | Socket | Pipe | File | Append_file | Synchronous_file
external kind_raw : Unix.file_descr -> kind = "caml_eio_windows_fd_kind"

(** Classify the handle. Standard descriptors always use synchronous I/O. *)
let kind fd =
  (* Inherited standard handles may belong to the parent's completion port. *)
  if fd == Unix.stdin || fd == Unix.stdout || fd == Unix.stderr then Synchronous
  else kind_raw fd

module Locks (Key : sig type t end) = struct
  module Table = Ephemeron.K1.Make(struct
      type t = Key.t
      let equal = ( == )
      let hash = Hashtbl.hash
    end)

  let table = Table.create 16
  let lock = Mutex.create ()
  let get key = Mutex.protect lock (fun () ->
      match Table.find_opt table key with
      | Some m -> m
      | None ->
        let m = Eio.Mutex.create () in
        Table.add table key m;
        m)
end

module Sync_locks = Locks(struct type t = Unix.file_descr end)
module File_locks = Locks(struct type t = Iocp.file_key end)

let file_lock h = File_locks.get (Iocp.file_key h)

module Sync_read = struct
  let lock = Mutex.create ()
  let readers = ref []

  (* Each entry borrows its descriptor through Fd.use until it leaves the list.
     Readers of the same pipe must hold one mutex from readiness through read. *)
  let with_reader fd fn =
    let entry = Mutex.protect lock (fun () ->
        let mutex = match List.find_opt (fun (other, _) ->
            fd == other || Iocp.same_handle fd other) !readers with
          | Some (_, mutex) -> mutex
          | None -> Eio.Mutex.create ()
        in
        let entry = fd, mutex in
        readers := entry :: !readers;
        entry)
    in
    Fun.protect ~finally:(fun () ->
        Mutex.protect lock (fun () -> readers := List.filter ((!=) entry) !readers))
      (fun () -> Eio.Mutex.use_ro (snd entry) fn)
end

external seek_raw : Unix.file_descr -> int64 -> int -> int64 = "caml_eio_windows_seek"
external reopen : Unix.file_descr -> bool -> Unix.file_descr = "caml_eio_windows_reopen"

(** Change the file position while excluding sequential I/O on this descriptor. *)
let seek resource fd off whence =
  let mutex = match kind fd with
    | File | Append_file -> file_lock (Completion.handle resource fd)
    | Synchronous | Synchronous_file | Socket | Pipe -> Sync_locks.get fd
  in
  Eio.Mutex.use_ro mutex (fun () -> seek_raw fd off whence)

(** Transfer data at the current position and advance it by the completed byte
    count, including when cancellation races with a successful transfer. *)
let sequential ~write ~append resource fd buf =
  let h = Completion.handle resource fd in
  Eio.Mutex.use_ro (file_lock h) @@ fun () ->
  let offset = if write && append then -1L else seek_raw fd 0L 1 in
  let on_complete (c : Iocp.completion) =
    if c.bytes_transferred > 0 then
      if offset = -1L then ignore (seek_raw fd 0L 2)
      else ignore (seek_raw fd (Int64.add offset (Int64.of_int c.bytes_transferred)) 0)
  in
  Completion.await ~on_complete (fun port ~data ->
      (if write then Iocp.write else Iocp.read) port ~data h buf ~offset)

(** Transfer data at [offset] without changing the descriptor's file position. *)
let transfer_at ~write resource fd buf ~offset =
  let run h = Completion.await (fun port ~data ->
      (if write then Iocp.write else Iocp.read) port ~data h buf ~offset)
  in
  match kind fd with
  | File | Append_file -> run (Completion.handle resource fd)
  | Synchronous | Synchronous_file | Socket | Pipe ->
    let fd = Eio_unix.run_in_systhread (fun () -> reopen fd write) in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> Completion.with_handle fd run)

let positioned ~write ~file_offset resource find_opt bufs =
  let offset = Optint.Int63.to_int64 file_offset in
  if offset < 0L then invalid_arg "Negative file offset";
  Eio_unix.Fd.use_exn "positioned_io" resource @@ fun fd ->
  match find_opt (fun buf -> not (Cstruct.is_empty buf)) bufs with
  | None -> 0
  | Some buf -> transfer_at ~write resource fd buf ~offset
