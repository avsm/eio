open Eio.Std

(* Byte I/O is dispatched per descriptor:

   - sockets and (socketpair-backed) pipes use overlapped IOCP operations
     ([recv]/[send]/[recv_from]/[send_to]), driven by {!Sched};
   - regular files and the blocking std handles (console) do a blocking call on
     the systhread pool, which doesn't stall the scheduler domain.

   Metadata operations (stat, readdir, rename, ...) and [openat] have no
   overlapped form and likewise run on the systhread pool. An INET [connect] uses
   ConnectEx so it can be cancelled, while an AF_UNIX [connect] is a blocking
   systhread call (see {!connect}); [accept] uses AcceptEx (see {!accept}) because
   it too must be cancellable. *)

let in_worker_thread = Eio_unix.run_in_systhread

module Fd = Eio_unix.Fd

(* [Iocp] operations take a [Handle.t]; for an already-associated descriptor this
   is just a view of the raw FD. *)
let h = Iocp.Handle.of_fd

(* How to perform stream byte-I/O on [fd]. *)
let classify fd =
  if Fd.is_blocking fd then `Blocking          (* console / std handle *)
  else if Fd.is_seekable fd then `File         (* regular file (synchronous handle) *)
  else `Socket                                 (* socket or socketpair pipe *)

(* An overlapped recv/send on a socket whose peer has gone away reports a
   Windows-specific code rather than the [ECONNRESET] a real reset would give.
   Normalise those to [ECONNRESET] so the flow layer treats them uniformly (a
   hard reset for real sockets, end-of-file for the socketpair-backed pipes — see
   {!Flow.Pipe_impl}). These codes are absent from the runtime's Win32->errno
   table, so [win32_maperr] passes them through as the raw (possibly negated)
   code, hence the [EUNKNOWNERR (-n)] forms. [ERROR_BROKEN_PIPE] is deliberately
   not listed: the runtime maps it to the named [Unix.EPIPE], which {!Flow} and
   {!Err} already treat as reset/EOF. *)
let normalise_error = function
  | Unix.EUNKNOWNERR (64 | -64)      (* ERROR_NETNAME_DELETED    — peer closed the socket *)
  | Unix.EUNKNOWNERR (1236 | -1236)  (* ERROR_CONNECTION_ABORTED *)
    -> Unix.ECONNRESET
  | e -> e

(* Errors that mean a read should report end-of-file rather than fail: a recv
   unblocked by [shutdown `Receive] aborts ([ERROR_OPERATION_ABORTED]), and a
   recv issued after the receive side is already shut down reports [ESHUTDOWN] —
   POSIX returns 0 (EOF) in both cases. (A fiber cancellation never reaches here;
   the scheduler reports it before calling this.) *)
let read_eof_error = function
  | Unix.EUNKNOWNERR (995 | -995)   (* ERROR_OPERATION_ABORTED *)
  | Unix.ESHUTDOWN
    -> true
  | _ -> false

(* Interpret a completion: the byte count on success, or raise on failure. *)
let bytes_exn ?(read = false) op (cs : Iocp.completion_status) =
  match cs.error with
  | None -> cs.bytes_transferred
  | Some e when read && read_eof_error e -> 0
  | Some e -> raise (Unix.Unix_error (normalise_error e, op, ""))

let sleep_until time =
  Sched.enter "sleep" @@ fun t k ->
  Sched.await_timeout t k time

let read_cstruct fd (buf : Cstruct.t) =
  Fd.use_exn "read_cstruct" fd @@ fun raw ->
  match classify fd with
  | `Socket ->
    if Cstruct.length buf = 0 then 0
    else bytes_exn ~read:true "read" (Sched.enter_io ~read:raw "read" (fun iocp -> Iocp.recv iocp (h raw) [buf]))
  | `Blocking | `File ->
    in_worker_thread (fun () -> Unix.read_bigarray raw buf.buffer buf.off buf.len)

let write_cstruct fd (buf : Cstruct.t) =
  Fd.use_exn "write_cstruct" fd @@ fun raw ->
  match classify fd with
  | `Socket ->
    if Cstruct.length buf = 0 then 0
    else bytes_exn "write" (Sched.enter_io "write" (fun iocp -> Iocp.send iocp (h raw) [buf]))
  | `Blocking | `File ->
    in_worker_thread (fun () -> Unix.write_bigarray raw buf.buffer buf.off buf.len)

(* The bytes API. On a socket the data must live in a pinned bigarray for the
   overlapped op, so we copy via a temporary cstruct; on the systhread (file /
   console) path we use the caller's [bytes] directly, avoiding a copy. *)
let read fd buf start len =
  Fd.use_exn "read" fd @@ fun raw ->
  match classify fd with
  | `Socket ->
    if len = 0 then 0
    else begin
      (* [create_unsafe]: [recv] overwrites the prefix and only that prefix is blitted back. *)
      let c = Cstruct.create_unsafe len in
      let n = bytes_exn ~read:true "read" (Sched.enter_io ~read:raw "read" (fun iocp -> Iocp.recv iocp (h raw) [c])) in
      Cstruct.blit_to_bytes c 0 buf start n;
      n
    end
  | `Blocking | `File ->
    in_worker_thread (fun () -> Unix.read raw buf start len)

let write fd buf start len =
  Fd.use_exn "write" fd @@ fun raw ->
  match classify fd with
  | `Socket ->
    if len = 0 then 0
    else begin
      (* [create_unsafe]: every byte is overwritten by the blit below. *)
      let c = Cstruct.create_unsafe len in
      Cstruct.blit_from_bytes buf start c 0 len;
      bytes_exn "write" (Sched.enter_io "write" (fun iocp -> Iocp.send iocp (h raw) [c]))
    end
  | `Blocking | `File ->
    in_worker_thread (fun () -> Unix.write raw buf start len)

let writev fd bufs =
  Fd.use_exn "writev" fd @@ fun raw ->
  match classify fd with
  | `Socket ->
    (* A single overlapped WSASend covers the whole vector; loop only if the
       kernel reports a short write (rare). *)
    let rec loop bufs =
      let n = bytes_exn "writev" (Sched.enter_io "writev" (fun iocp -> Iocp.send iocp (h raw) bufs)) in
      match Cstruct.shiftv bufs n with
      | [] -> ()
      | rest -> assert (n > 0); loop rest   (* n = 0 here would mean no progress *)
    in
    if Cstruct.lenv bufs > 0 then loop bufs
  | `Blocking | `File ->
    in_worker_thread (fun () ->
        List.iter (fun (buf : Cstruct.t) ->
            let rec loop off len =
              if len > 0 then begin
                let n = Unix.write_bigarray raw buf.buffer off len in
                loop (off + n) (len - n)
              end
            in
            loop buf.off buf.len)
          bufs)

let socket ~sw socket_domain socket_type protocol =
  Switch.check sw;
  let sock_unix = Unix.socket ~cloexec:true socket_domain socket_type protocol in
  (* Leave the OS socket in blocking mode: overlapped IOCP ops and ConnectEx ignore
     it, but the AF_UNIX [connect] fallback is a blocking systhread call. The [Fd]
     is tagged [~blocking:false] — a separate notion — so {!classify} routes it to
     the IOCP socket path rather than the systhread/console path. *)
  match Sched.associate (Sched.get ()) sock_unix with
  | () -> Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix:true sock_unix
  | exception e -> (try Unix.close sock_unix with Unix.Unix_error _ -> ()); raise e

let connect fd addr =
  Fd.use_exn "connect" fd @@ fun raw ->
  match addr with
  | Unix.ADDR_INET _ ->
    (* ConnectEx: an overlapped, cancellable connect. It requires the socket to be
       bound first, so bind to the wildcard address of the matching family. *)
    let sched = Sched.get () in
    let any =
      match Unix.domain_of_sockaddr addr with
      | Unix.PF_INET6 -> Unix.inet6_addr_any
      | _ -> Unix.inet_addr_any
    in
    Unix.bind raw (Unix.ADDR_INET (any, 0));
    let cs = Sched.enter_io "connect" (fun iocp -> Iocp.connect iocp (h raw) (Iocp.Sockaddr.of_unix addr)) in
    (match cs.error with
     | Some e -> raise (Unix.Unix_error (normalise_error e, "connect", ""))
     | None -> ());
    Iocp.update_connect_ctx (Sched.iocp sched) (h raw)
  | Unix.ADDR_UNIX _ ->
    (* ConnectEx also needs a bound socket, but Windows AF_UNIX has no autobind, so
       it would force the client to bind a named path — leaving a stray socket file
       and an un-anonymous client. A blocking connect on the systhread pool keeps the
       client anonymous, and AF_UNIX connects are local so blocking is cheap. *)
    in_worker_thread (fun () -> Unix.connect raw addr)

let accept ~sw sock =
  Switch.check sw;
  (* AcceptEx: pre-create an accept socket of the same family, associate it, and
     submit an overlapped accept. Unlike a blocking accept this is cancellable
     (via [CancelIoEx]), which the server accept-loop relies on. *)
  Fd.use_exn "accept" sock @@ fun listen_raw ->
  let domain = Unix.domain_of_sockaddr (Unix.getsockname listen_raw) in
  let accept_u = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  match
    let t = Sched.get () in
    Sched.associate t accept_u;
    let addr_buf = Iocp.Sockaddr.accept_buffer () in
    let cs = Sched.enter_io "accept" (fun iocp -> Iocp.accept iocp (h listen_raw) (h accept_u) addr_buf) in
    (match cs.error with
     | Some e -> raise (Unix.Unix_error (normalise_error e, "accept", ""))
     | None -> ());
    Iocp.update_accept_ctx (Sched.iocp t) ~listen:(h listen_raw) (h accept_u);
    Iocp.Sockaddr.get (Iocp.Sockaddr.of_accept_buffer addr_buf ~listen:(h listen_raw))
  with
  | addr -> Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix:true accept_u, addr
  | exception e -> (try Unix.close accept_u with Unix.Unix_error _ -> ()); raise e

let shutdown sock cmd =
  Fd.use_exn "shutdown" sock @@ fun raw ->
  Unix.shutdown raw cmd;
  (* Unlike POSIX [SHUT_RD], a Windows [SD_RECEIVE] doesn't complete an overlapped
     recv already in flight. Cancel just the in-flight reads on this handle so a
     blocked reader wakes up; [bytes_exn ~read] then turns the resulting abort into
     end-of-file. Cancelling only reads (not the whole handle) leaves a concurrent
     send/writev running, matching POSIX shutdown. *)
  match cmd with
  | Unix.SHUTDOWN_RECEIVE | Unix.SHUTDOWN_ALL ->
    Sched.cancel_reads (Sched.get ()) raw
  | Unix.SHUTDOWN_SEND -> ()

(* Datagrams are vectored: eio hands us bigarray-backed cstructs, which the
   overlapped op can pin and scatter/gather directly, so no copy is needed (the
   temporary-cstruct dance in the [bytes] API above exists only because [bytes]
   are GC-movable — that reasoning does not apply here). *)
let send_msg fd ?dst bufs =
  Fd.use_exn "send_msg" fd @@ fun raw ->
  let cs =
    match dst with
    | Some dst -> Sched.enter_io "send_msg" (fun iocp -> Iocp.send_to iocp (h raw) bufs (Iocp.Sockaddr.of_unix dst))
    | None     -> Sched.enter_io "send_msg" (fun iocp -> Iocp.send iocp (h raw) bufs)
  in
  bytes_exn "send_msg" cs

let recv_msg fd bufs =
  Fd.use_exn "recv_msg" fd @@ fun raw ->
  let addr = Iocp.Sockaddr.create () in
  let cs = Sched.enter_io ~read:raw "recv_msg" (fun iocp -> Iocp.recv_from iocp (h raw) bufs addr) in
  (* A datagram recv has no EOF: let a shutdown/abort raise (as eio_posix does)
     rather than return a 0-byte read with an unfilled source address. *)
  let n = bytes_exn "recv_msg" cs in
  (n, Iocp.Sockaddr.get addr)

external eio_getrandom : Cstruct.buffer -> int -> int -> int = "caml_eio_windows_getrandom"

let getrandom { Cstruct.buffer; off; len } =
  let rec loop n =
    if n = len then
      ()
    else
      loop (n + eio_getrandom buffer (off + n) (len - n))
  in
  in_worker_thread @@ fun () ->
  loop 0

(* Convert a [Unix.LargeFile.stats] (from [fstat]'s socket/pipe fallback or from
   [lstat]) to an Eio stat. The [blksize]/[blocks] fields aren't available this
   way, hence the zeroes; the native [fstat] path below fills them in. *)
let eio_stat (ust : Unix.LargeFile.stats) : Eio.File.Stat.t =
  let st_kind : Eio.File.Stat.kind =
    match ust.st_kind with
    | Unix.S_REG  -> `Regular_file
    | Unix.S_DIR  -> `Directory
    | Unix.S_CHR  -> `Character_special
    | Unix.S_BLK  -> `Block_device
    | Unix.S_LNK  -> `Symbolic_link
    | Unix.S_FIFO -> `Fifo
    | Unix.S_SOCK -> `Socket
  in
  Eio.File.Stat.{
    dev     = ust.st_dev   |> Int64.of_int;
    ino     = ust.st_ino   |> Int64.of_int;
    kind    = st_kind;
    perm    = ust.st_perm;
    nlink   = ust.st_nlink |> Int64.of_int;
    uid     = ust.st_uid   |> Int64.of_int;
    gid     = ust.st_gid   |> Int64.of_int;
    rdev    = ust.st_rdev  |> Int64.of_int;
    size    = ust.st_size  |> Optint.Int63.of_int64;
    blksize = 0L;   (* Not available via Unix stat on Windows *)
    blocks  = 0L;   (* Not available via Unix stat on Windows *)
    atime   = ust.st_atime;
    mtime   = ust.st_mtime;
    ctime   = ust.st_ctime;
  }

(* A by-handle [GetFileInformationByHandle]-based stat, or [None] for a non-disk
   handle (socket/pipe/console), where the caller falls back to [Unix] stat. The
   tuple order matches the C stub. *)
external eio_fstat :
  Unix.file_descr ->
  (int * int64 * int64 * int64 * int64 * int * float * float * float * int64 * int64) option
  = "caml_eio_windows_fstat"

let kind_of_int : int -> Eio.File.Stat.kind = function
  | 1 -> `Directory
  | 2 -> `Symbolic_link
  | _ -> `Regular_file

(* [fstat] runs on the systhread pool: the native query can touch the disk, and
   this keeps sockets/pipes on the same fallback path as the other metadata ops. *)
let fstat fd =
  in_worker_thread @@ fun () ->
  Fd.use_exn "fstat" fd @@ fun raw ->
  match eio_fstat raw with
  | None -> eio_stat (Unix.LargeFile.fstat raw)   (* socket / pipe / console *)
  | Some (kind, dev, ino, nlink, size, perm, atime, mtime, ctime, blksize, blocks) ->
    Eio.File.Stat.{
      dev; ino;
      kind = kind_of_int kind;
      perm;
      nlink;
      uid = 0L; gid = 0L; rdev = 0L;   (* meaningless on Windows *)
      size = Optint.Int63.of_int64 size;
      blksize;
      blocks;
      atime; mtime; ctime;
    }

let lstat path =
  in_worker_thread @@ fun () ->
  Unix.LargeFile.lstat path

let realpath path =
  in_worker_thread @@ fun () ->
  Unix.realpath path

external eio_readdir : Unix.file_descr -> (Eio.File.Stat.kind * string) list = "caml_eio_windows_readdir"

(* Enumerate [fd] (a directory handle) in a single pass, returning each entry's
   name together with its kind — no per-entry [stat], so no N+1 syscalls and no
   race with concurrent modification. Runs on the systhread pool like other
   metadata ops (the stub blocks in [GetFileInformationByHandleEx]). *)
let read_dir_entries fd =
  Fd.use_exn "readdir" fd @@ fun raw ->
  in_worker_thread @@ fun () -> eio_readdir raw

let read_link ?dirfd path =
  in_worker_thread @@ fun () ->
  Eio_unix.Private.read_link dirfd path

(* Windows has no [chown]; raise [EOPNOTSUPP] on both paths (the dirfd stub
   already does) so [Path.chown] fails uniformly and catchably. *)
let chown ?dirfd ~follow:_ ?(uid=(-1L)) ?(gid=(-1L)) path =
  in_worker_thread @@ fun () ->
  match dirfd with
  | None -> raise (Unix.Unix_error (Unix.EOPNOTSUPP, "chown", path))
  | Some dirfd -> Eio_unix.Private.chown ~flags:0 ~uid ~gid dirfd path

external eio_preadv : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int = "caml_eio_windows_preadv"
external eio_pwritev : Unix.file_descr -> Cstruct.t array -> Optint.Int63.t -> int = "caml_eio_windows_pwritev"

(* Positioned file I/O. The stub blocks in [ReadFile]/[WriteFile] (releasing the
   runtime lock), so we run it on the systhread pool rather than stalling the
   scheduler domain. *)
let preadv ~file_offset fd bufs =
  Fd.use_exn "preadv" fd @@ fun raw ->
  in_worker_thread (fun () -> eio_preadv raw bufs file_offset)

let pwritev ~file_offset fd bufs =
  Fd.use_exn "pwritev" fd @@ fun raw ->
  in_worker_thread (fun () -> eio_pwritev raw bufs file_offset)

module Flags = struct
  module Open = struct
    type t = int
    let rdonly = Config.o_rdonly
    let rdwr = Config.o_rdwr
    let wronly = Config.o_wronly
    let cloexec = Config.o_noinherit
    let creat = Config.o_creat
    let excl = Config.o_excl
    let trunc = Config.o_trunc

    let generic_read = Config.generic_read
    let generic_write = Config.generic_write
    let synchronise = Config.synchronize
    let append = Config.file_append_data

    let empty = 0
    let ( + ) = ( lor )
  end

  module Disposition = struct
    type t = int
    let supersede = Config.file_supersede
    let create = Config.file_create
    let open_ = Config.file_open
    let open_if = Config.file_open_if
    let overwrite = Config.file_overwrite
    let overwrite_if = Config.file_overwrite_if
  end

  module Create = struct
    type t = int
    let empty = 0
    let directory = Config.file_directory_file
    let non_directory = Config.file_non_directory_file
    let no_intermediate_buffering = Config.file_no_intermediate_buffering
    let write_through = Config.file_write_through
    let sequential_only = Config.file_sequential_only
    let ( + ) = ( lor )
  end
end

let with_dirfd op dirfd fn =
  match dirfd with
  | None -> fn None
  | Some dirfd -> Fd.use_exn op dirfd (fun fd -> fn (Some fd))

external eio_openat : Unix.file_descr option -> bool -> string -> Flags.Open.t -> Flags.Disposition.t -> Flags.Create.t -> Unix.file_descr = "caml_eio_windows_openat_bytes" "caml_eio_windows_openat"

let openat ?dirfd ?(nofollow=false) ~sw path flags dis create =
  with_dirfd "openat" dirfd @@ fun dirfd ->
  Switch.check sw;
  in_worker_thread ~label:"openat" (fun () -> eio_openat dirfd nofollow path Flags.Open.(flags + cloexec) dis create)
  |> Fd.of_unix ~sw ~blocking:false ~seekable:true ~close_unix:true

let mkdir ?dirfd ?(nofollow=false) ~mode:_ path =
  Switch.run @@ fun sw ->
  let _ : Fd.t = openat ?dirfd ~nofollow ~sw path Flags.Open.(generic_write + synchronise) Flags.Disposition.(create) Flags.Create.(directory) in
  ()

external eio_unlinkat : Unix.file_descr option -> string -> bool -> unit = "caml_eio_windows_unlinkat"

let unlink ?dirfd ~dir path =
  with_dirfd "unlink" dirfd @@ fun dirfd ->
  in_worker_thread ~label:"unlink" @@ fun () ->
  eio_unlinkat dirfd path dir

external eio_renameat : Unix.file_descr option -> string -> Unix.file_descr option -> string -> unit = "caml_eio_windows_renameat"

let rename ?old_dir old_path ?new_dir new_path =
  with_dirfd "rename-old" old_dir @@ fun old_dir ->
  with_dirfd "rename-new" new_dir @@ fun new_dir ->
  in_worker_thread ~label:"rename" @@ fun () ->
  eio_renameat old_dir old_path new_dir new_path


(* Resolve [dir / leaf] to an absolute path; with no [dir] handle the leaf is
   already usable. *)
external eio_path_at : Unix.file_descr option -> string -> string = "caml_eio_windows_path_at"

let symlink ~link_to new_dir new_path =
  (* Creating symlinks on Windows needs Developer Mode or
     [SeCreateSymbolicLinkPrivilege]; report the lack as "not supported" rather
     than failing later with an obscure privilege error. *)
  if not (Unix.has_symlink ()) then
    raise (Unix.Unix_error (Unix.EOPNOTSUPP, "symlink", new_path));
  with_dirfd "symlink-new" new_dir @@ fun new_dir ->
  in_worker_thread ~label:"symlink" @@ fun () ->
  let target = eio_path_at new_dir new_path in
  (* Windows distinguishes file and directory symlinks; pick based on the target. *)
  let to_dir = try Sys.is_directory link_to with Sys_error _ -> false in
  Unix.symlink ~to_dir link_to target

let chmod ~mode new_dir new_path =
  with_dirfd "chmod" new_dir @@ fun new_dir ->
  match new_dir with
  | Some _ -> raise (Unix.Unix_error (Unix.EOPNOTSUPP, "chmod", new_path))
  | None ->
    in_worker_thread ~label:"chmod" @@ fun () ->
    Unix.chmod new_path mode

(* [Unix.LargeFile.lseek] returns garbage on the raw NtCreateFile handles the
   backend hands out (see {!openat}), so seek natively with [SetFilePointerEx].
   [cmd] is encoded as 0/1/2 for SEEK_SET/CUR/END. *)
external eio_lseek : Unix.file_descr -> int64 -> int -> int64 = "caml_eio_windows_lseek"

let lseek fd off cmd =
  Fd.use_exn "lseek" fd @@ fun fd ->
  let cmd =
    match cmd with
    | `Set -> 0
    | `Cur -> 1
    | `End -> 2
  in
  eio_lseek fd (Optint.Int63.to_int64 off) cmd
  |> Optint.Int63.of_int64

let fsync fd =
  in_worker_thread @@ fun () ->
  Fd.use_exn "fsync" fd Unix.fsync

let ftruncate fd len =
  in_worker_thread @@ fun () ->
  Fd.use_exn "ftruncate" fd @@ fun fd ->
  Unix.LargeFile.ftruncate fd (Optint.Int63.to_int64 len)

let pipe ~sw =
  (* Windows anonymous pipes can't be made non-blocking or used with overlapped
     I/O, so emulate a pipe with a (bidirectional) socketpair, whose ends go
     through IOCP like any other socket. Association is left to the caller: the
     [Pipe] effect handler creates the pipe from within an effect handler, where
     [Sched.get] (itself an effect) can't be performed, so it associates the ends
     using its own in-scope scheduler. *)
  let unix_r, unix_w = Sched.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let r = Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix:true unix_r in
  let w = Fd.of_unix ~sw ~blocking:false ~seekable:false ~close_unix:true unix_w in
  r, w
