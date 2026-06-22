(** A [_ Path.t] represents a particular location in some filesystem.
    It is a pair of a base directory and a relative path from there.

    {!Eio.Stdenv.cwd} provides access to the current working directory.
    For example:

    {[
      let ( / ) = Eio.Path.( / )

      let run dir =
        Eio.Path.save ~create:(`Exclusive 0o600)
          (dir / "output.txt") "the data"

      let () =
        Eio_main.run @@ fun env ->
        run (Eio.Stdenv.cwd env)
    ]}

    It is normally not permitted to access anything above the base directory,
    even by following a symlink.
    The exception is {!Stdenv.fs}, which provides access to the whole file-system:

    {[
      Eio.Path.load (fs / "/etc/passwd")
    ]}

    In Eio, the directory separator is always "/", even on Windows.
    Use {!native} to convert to a native path.
*)

open Std
open Fs

type 'a t = 'a Fs.dir * path
(** An OS directory FD and a path relative to it, for use with e.g. [openat(2)]. *)

val ( / ) : 'a t -> string -> 'a t
(** [t / step] is [t] with [step] appended to [t]'s path,
    or replacing [t]'s path if [step] is absolute:

    - [(fd, "foo") / "bar" = (fd, "foo/bar")]
    - [(fd, "foo") / "/bar" = (fd, "/bar")] *)

val pp : _ t Fmt.t
(** [pp] formats a [_ t] as "<label:path>", suitable for logging. *)

val native : _ t -> string option
(** [native t] returns a path that can be used to refer to [t] with the host
    platform's native string-based file-system APIs, if available.
    This is intended for interoperability with non-Eio libraries.

    This does not check for confinement (the resulting path might not be accessible
    via [t] itself). Also, if a directory was opened with {!open_subtree} and later
    renamed, this might use the old name.

    Using strings as paths is not secure if components in the path can be
    replaced by symlinks while the path is being used. For example, if you
    try to write to "/home/mal/output.txt" just as mal replaces "output.txt"
    with a symlink to "/etc/passwd". *)

val native_exn : _ t -> string
(** Like {!native}, but raise a suitable exception if the path is not a native path. *)

val split : 'a t -> ('a t * string) option
(** [split t] returns [Some (dir, basename)], where [basename] is the last path component in [t]
    and [dir] is [t] without [basename].

    [dir / basename] refers to the same path as [t].

    [split t = None] if there is nothing to split.

    For example:

    - [split (root, "foo/bar") = Some ((root, "foo"), "bar")]
    - [split (root, "/foo/bar") = Some ((root, "/foo"), "bar")]
    - [split (root, "/foo/bar/baz") = Some ((root, "/foo/bar"), "baz")]
    - [split (root, "/foo/bar//baz/") = Some ((root, "/foo/bar"), "baz")]
    - [split (root, "bar") = Some ((root, ""), "bar")]
    - [split (root, ".") = Some ((root, ""), ".")]
    - [split (root, "") = None]
    - [split (root, "/") = None]
*)

(** {1 Reading files} *)

val load : _ t -> string
(** [load t] returns the contents of the given file.

    This is a convenience wrapper around {!with_open_in}. *)

val open_in : sw:Switch.t -> _ t -> File.ro_ty r
(** [open_in ~sw t] opens [t] for reading.

    Note: files are always opened in binary mode. *)

val with_open_in : _ t -> (File.ro_ty r -> 'a) -> 'a
(** [with_open_in] is like [open_in], but calls [fn flow] with the new flow and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

val with_lines : _ t -> (string Seq.t -> 'a) -> 'a
(** [with_lines t fn] is a convenience function for streaming the lines of the file.

    It uses {!Buf_read.lines}. *)

(** {1 Writing files} *)

val save : ?append:bool -> ?atomic:bool -> create:create -> _ t -> string -> unit
(** [save t data ~create] writes [data] to [t].

    This is a convenience wrapper around {!with_open_out}.

    @param atomic See {!with_open_out}. *)

val open_out :
  sw:Switch.t ->
  ?append:bool ->
  create:create ->
  _ t -> File.rw_ty Resource.t
(** [open_out ~sw t] opens [t] for reading and writing.

    Note: files are always opened in binary mode.
    @param append Open for appending: always write at end of file.
    @param create Controls whether to create the file, and what permissions to give it if so. *)

val with_open_out :
  ?append:bool ->
  ?atomic:bool ->
  create:create ->
  _ t -> (File.rw_ty r -> 'a) -> 'a
(** [with_open_out] is like [open_out], but calls [fn flow] with the new flow and closes
    it automatically when [fn] returns (if it hasn't already been closed by then). *)

(** {1 Directories} *)

val mkdir : perm:File.Unix_perm.t -> _ t -> unit
(** [mkdir ~perm t] creates a new directory [t] with permissions [perm]. *)

val mkdirs : ?exists_ok:bool -> perm:File.Unix_perm.t -> _ t -> unit
(** [mkdirs ~perm t] creates directory [t] along with any missing ancestor directories, recursively.

    All created directories get permissions [perm], but existing directories do not have their permissions changed.

    @param exist_ok If [false] (the default) then we raise {! Fs.Already_exists} if [t] is already a directory. *)

val open_subtree : sw:Switch.t -> _ t -> [< `Close | dir_ty] t
(** [open_subtree ~sw t] returns a path that grants access only to the subtree at [t].

    The returned path will not allow use of ".." or symlinks to escape the subtree.

    @since 1.4 *)

val open_dir : sw:Switch.t -> _ t -> [< `Close | dir_ty] t
(** Deprecated alias of {!open_subtree}. *)

val with_subtree : _ t -> ([< `Close | dir_ty] t -> 'a) -> 'a
(** [with_subtree] is like [open_subtree], but calls [fn dir] with the new directory and closes
    it automatically when [fn] returns (if it hasn't already been closed by then).

    @since 1.4 *)

val with_open_dir : _ t -> ([< `Close | dir_ty] t -> 'a) -> 'a
(** Deprecated alias of {!with_subtree}. *)

val read_dir : _ t -> string list
(** [read_dir t] reads directory entry names for [t].

    The entries are sorted using {! String.compare}.

    Note: The special Unix entries "." and ".." are not included in the results. *)

val read_dir_entries : _ t -> (File.Stat.kind * string) list
(** [read_dir_entries] is like {!read_dir}, but also includes the kind of each entry.

    This is typically much faster than calling {!kind} on each entry individually. *)

val with_dir_entries : _ t -> ((File.Stat.kind * string) Seq.t -> 'a) -> 'a
(** [with_dir_entries t fn] runs [fn items], where [items] is the entries as a sequence.

    This is like {!read_dir_entries}, but loads the entries incrementally,
    which may be more efficient. Unlike {!read_dir_entries}, it does not sort
    the results, which may be returned in any order. *)

(** {1 Temporary files and directories}

    These create fresh, uniquely-named entries within an existing directory,
    using the operating system's secure primitives to pick an unpredictable
    name. To use the system temporary directory as the parent, combine
    {!Eio.Stdenv.fs} with [Filename.get_temp_dir_name]:

    {[
      let tmp = Eio.Stdenv.fs env / Filename.get_temp_dir_name () in
      Eio.Path.with_tmp_dir tmp @@ fun dir ->
      ...
    ]} *)

val with_tmp_file : ?perm:File.Unix_perm.t -> _ t -> (File.rw_ty r -> 'a) -> 'a
(** [with_tmp_file dir fn] creates a fresh, anonymous temporary file within [dir],
    open for reading and writing, and calls [fn file] with it.

    The file has no name: it is not linked into [dir], so it never appears in
    {!read_dir} and cannot be reached by any path. It is accessible only through
    the flow passed to [fn], and its storage is reclaimed automatically when the
    flow is closed (when [fn] returns or raises) or when the process exits. This
    makes it well suited to scratch data that might outgrow memory.

    On Linux the file is created directly as an unnamed inode ([O_TMPFILE]); on
    other Unix systems it is created and then unlinked immediately. Either way it
    is anonymous from the moment [fn] runs. On Windows, which cannot unlink an
    open file, the underlying name persists until the file is closed.

    @param perm The permissions for the new file (default [0o600]).
    @since 1.4 *)

val open_tmp_file : sw:Switch.t -> ?perm:File.Unix_perm.t -> _ t -> File.rw_ty r
(** [open_tmp_file ~sw dir] is like {!with_tmp_file}, but the file is closed (and
    so removed) when [sw] finishes rather than at the end of a function.

    @since 1.4 *)

val with_tmp_dir :
  ?cleanup:bool -> ?perm:File.Unix_perm.t -> ?prefix:string -> ?suffix:string ->
  _ t -> ([< `Close | dir_ty] t -> 'a) -> 'a
(** [with_tmp_dir dir fn] creates a fresh, uniquely-named temporary directory
    within [dir], calls [fn tmp] with it, and removes it (recursively) when [fn]
    returns or raises.

    @param cleanup If [true] (the default), remove the directory and its contents
                   when the switch finishes.
    @param perm The permissions for the new directory (default [0o700]).
    @param prefix Prepended to the random name (default [""]).
    @param suffix Appended to the random name (default [""]).
    @since 1.4 *)

val open_tmp_dir :
  sw:Switch.t -> ?cleanup:bool -> ?perm:File.Unix_perm.t -> ?prefix:string -> ?suffix:string ->
  _ t -> [< `Close | dir_ty] t
(** [open_tmp_dir ~sw dir] is like {!with_tmp_dir}, but the directory is removed
    when [sw] finishes rather than at the end of a function.

    @since 1.4 *)

(** {1 Metadata} *)

val stat : follow:bool -> _ t -> File.Stat.t
(** [stat ~follow t] returns metadata about the file [t].

    If [t] is a symlink, the information returned is about the target if [follow = true],
    otherwise it is about the link itself. *)

val kind : follow:bool -> _ t -> [ File.Stat.kind | `Not_found ]
(** [kind ~follow t] is the type of [t], or [`Not_found] if it doesn't exist.

    @param follow If [true] and [t] is a symlink, return the type of the target rather than [`Symbolic_link]. *)

val is_file : _ t -> bool
(** [is_file t] is [true] if [t] is a regular file, and [false] if it doesn't exist or has a different type.

    [is_file t] is [kind ~follow:true t = `Regular_file]. *)

val is_directory : _ t -> bool
(** [is_directory t] is [true] if [t] is a directory, and [false] if it doesn't exist or has a different type.

    [is_directory t] is [kind ~follow:true t = `Directory]. *)

val read_link : _ t -> string
(** [read_link t] is the target of symlink [t]. *)

(** {1 Other} *)

val unlink : ?missing_ok:bool -> _ t -> unit
(** [unlink t] removes directory entry [t].

    @param missing_ok If [false] (the default), raise an {!Fs.Not_found} IO error if [t] doesn't exist.
                      If [true], do nothing if [t] is missing.

    Note: this cannot be used to unlink directories.
    Use {!rmdir} for directories. *)

val rmdir : _ t -> unit
(** [rmdir t] removes directory entry [t].
    This only works when the entry is itself a directory.

    Note: this usually requires the directory to be empty. *)

val rmtree : ?missing_ok:bool -> _ t -> unit
(** [rmtree t] removes [t] (and its contents, recursively, if it's a directory).

    @param missing_ok If [false] (the default), raise an {!Fs.Not_found} IO error if [t] doesn't exist.
                      If [true], ignore missing items.
                      This applies recursively, allowing two processes
                      to attempt to remove a tree at the same time. *)

val rename : _ t -> _ t -> unit
(** [rename old_t new_t] atomically unlinks [old_t] and links it as [new_t].

    If [new_t] already exists, it is atomically replaced. *)

val symlink : link_to:string -> _ t -> unit
(** [symlink ~link_to t] creates a symbolic link [t] to [link_to].

    [t] is the symlink that is created and [link_to] is the name used in the link.
    For example, this creates a "current" symlink pointing at "version-1.0":

    {[
      Eio.Path.symlink (dir / "current") ~link_to:"version-1.0"
    ]} *)

val chmod : follow:bool -> perm:File.Unix_perm.t -> _ t -> unit
(** [chmod ~follow ~perm t] allows you to change the file mode bits.

    @param follow If [true] and [t] is a symlink then change the target's mode bits. *)
