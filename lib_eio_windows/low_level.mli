(** This module provides an effects-based API for the Windows backend's I/O.

    Normally it's better to use the cross-platform {!Eio} APIs instead,
    which use these functions automatically where appropriate.

    Byte I/O is dispatched per descriptor: sockets and socketpair-backed pipes use
    overlapped IOCP operations driven by {!Sched}, while regular files and the
    blocking standard handles run on the systhread pool. These functions:

    + suspend the calling fiber rather than blocking the scheduler domain;
    + wrap {!Unix.file_descr} in {!Fd}, to avoid use-after-close bugs;
    + attach new FDs to switches, to avoid resource leaks. *)

open Eio.Std

type fd := Eio_unix.Fd.t

val sleep_until : Mtime.t -> unit

val read : fd -> bytes -> int -> int -> int
val read_cstruct : fd -> Cstruct.t -> int
val write : fd -> bytes -> int -> int -> int
val write_cstruct : fd -> Cstruct.t -> int

val socket : sw:Switch.t -> Unix.socket_domain -> Unix.socket_type -> int -> fd
val connect : fd -> Unix.sockaddr -> unit
val accept : sw:Switch.t -> fd -> fd * Unix.sockaddr

val shutdown : fd -> Unix.shutdown_command -> unit

val recv_msg : fd -> Cstruct.t list -> int * Unix.sockaddr
val send_msg : fd -> ?dst:Unix.sockaddr -> Cstruct.t list -> int

val getrandom : Cstruct.t -> unit

val lseek : fd -> Optint.Int63.t -> [`Set | `Cur | `End] -> Optint.Int63.t
val fsync : fd -> unit
val ftruncate : fd -> Optint.Int63.t -> unit

val eio_stat : Unix.LargeFile.stats -> Eio.File.Stat.t
(** Convert a {!Unix.LargeFile.stats} to an Eio stat (used for the [lstat] path
    and the socket/pipe [fstat] fallback). *)

val fstat : fd -> Eio.File.Stat.t
(** Native by-handle stat: on a disk handle it queries
    [GetFileInformationByHandle] directly (real dev/ino, 100ns times, link count,
    block usage); on a socket/pipe it falls back to {!Unix.LargeFile.fstat}. *)

val lstat : string -> Unix.LargeFile.stats

val realpath : string -> string
val read_link : ?dirfd:fd -> string -> string
val chown : ?dirfd:fd -> follow:bool -> ?uid:int64 -> ?gid:int64 -> string -> unit

val mkdir : ?dirfd:fd -> ?nofollow:bool -> mode:int -> string -> unit
val unlink : ?dirfd:fd -> dir:bool -> string -> unit
val rename : ?old_dir:fd -> string -> ?new_dir:fd -> string -> unit

val symlink : link_to:string -> fd option -> string -> unit
(** [symlink ~link_to dir path] will create a new symlink at [dir / path]
    linking to [link_to]. *)

val chmod : mode:int -> fd option -> string -> unit
(** [chmod ~mode fd path] runs {!Unix.chmod} on the systhread pool when
    [fd = None]; with a directory handle it raises [EOPNOTSUPP]. *)

val read_dir_entries : fd -> (Eio.File.Stat.kind * string) list
(** [read_dir_entries fd] enumerates the directory referred to by [fd] in a
    single pass, returning each entry's kind and name together (skipping [.] and
    [..]). The order is unspecified. *)

val writev : fd -> Cstruct.t list -> unit

val preadv : file_offset:Optint.Int63.t -> fd -> Cstruct.t array -> int
val pwritev : file_offset:Optint.Int63.t -> fd -> Cstruct.t array -> int

val pipe : sw:Switch.t -> fd * fd

module Flags : sig
  module Open : sig
    type t

    val rdonly : t
    val rdwr : t
    val wronly : t
    val creat : t
    val excl : t
    val trunc : t

    val generic_read : t
    val generic_write : t
    val synchronise : t
    val append : t

    val empty : t
    val ( + ) : t -> t -> t
  end

  module Disposition : sig
    type t

    val supersede : t
    (** If the file already exists, replace it with the given file.
        If it does not, create the given file. *)

    val create : t
    (** Create the file, if it already exists fail. *)

    val open_ : t
    (** If the file already exists, open it otherwise fail. *)

    val open_if : t
    (** If the file already exists, open it otherwise create it. *)

    val overwrite : t
    (** If the file already exists, open it and overwrite it otherwise fail. *)

    val overwrite_if : t
    (** If the file already exists, open it and overwrite it otherwise create it. *)
  end

  module Create : sig
    type t

    val empty : t
    (** No constraint on the kind of object opened (e.g. for [stat], which must
        work on both files and directories). *)

    val directory : t
    (** Create a directory. *)

    val non_directory : t
    (** Create something that is not a directory. *)

    val no_intermediate_buffering : t

    val write_through : t

    val sequential_only : t

    val ( + ) : t -> t -> t
  end
end

val openat : ?dirfd:fd -> ?nofollow:bool-> sw:Switch.t -> string -> Flags.Open.t -> Flags.Disposition.t -> Flags.Create.t -> fd
(** Note: the FD is close-on-exec and tagged non-blocking, though the underlying
    handle is actually synchronous (see the caveat at the top of [low_level.ml]). *)
