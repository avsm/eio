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

(* This module provides (optional) sandboxing, allowing operations to be restricted to a subtree.

   For now, sandboxed directories use realpath and [O_NOFOLLOW], which is probably quite slow,
   and requires duplicating a load of path lookup logic from the kernel.
   It might be better to hold a directory FD rather than a path.
   On FreeBSD we could use O_RESOLVE_BENEATH and let the OS handle everything for us.
   On other systems we would have to resolve one path component at a time. *)

open Eio.Std

module Fd = Eio_unix.Fd

module rec Dir : sig
  include Eio.Fs.Pi.DIR

  val v : label:string -> sandbox:bool -> string -> t

  val resolve : t -> string -> string
  (** [resolve t path] returns the real path that should be used to access [path].
      For sandboxes, this is [realpath path] (and it checks that it is within the sandbox).
      For unrestricted access, this returns [path] unchanged.
      @raise Eio.Fs.Permission_denied if sandboxed and [path] is outside of [dir_path]. *)

  val strip_nt_prefix : string -> string
  (** [strip_nt_prefix p] removes the [\??\] NT-namespace prefix that {!resolve}
      adds, yielding a plain Win32 path (e.g. for [Unix.lstat] or [CreateProcessW]). *)

  val with_parent_dir : t -> string -> (Fd.t option -> string -> 'a) -> 'a
  (** [with_parent_dir t path fn] runs [fn dir_fd rel_path],
      where [rel_path] accessed relative to [dir_fd] gives access to [path].
      For unrestricted access, this just runs [fn None path].
      For sandboxes, it opens the parent of [path] as [dir_fd] and runs [fn (Some dir_fd) (basename path)]. *)
end = struct
  type t = {
    dir_path : string;
    sandbox : bool;
    label : string;
    mutable closed : bool;
  }

  let resolve t path =
    if t.sandbox then (
      if t.closed then Fmt.invalid_arg "Attempt to use closed directory %S" t.dir_path;
      if Filename.is_relative path then (
        let dir_path = Err.run Low_level.realpath t.dir_path in
        let full = Err.run Low_level.realpath (Filename.concat dir_path path) in
        let prefix_len = String.length dir_path + 1 in
        (* \\??\\ Is necessary with NtCreateFile. *)
        if String.length full >= prefix_len && String.sub full 0 prefix_len = dir_path ^ Filename.dir_sep then begin
          "\\??\\" ^ full
        end else if full = dir_path then
          "\\??\\" ^ full
        else
          raise @@ Eio.Fs.err (Permission_denied (Err.Outside_sandbox (full, dir_path)))
      ) else (
        raise @@ Eio.Fs.err (Permission_denied Err.Absolute_path)
      )
    ) else path

  let with_parent_dir t path fn =
    if t.sandbox then (
      if t.closed then Fmt.invalid_arg "Attempt to use closed directory %S" t.dir_path;
      let dir, leaf = Filename.dirname path, Filename.basename path in
      if leaf = ".." then (
        (* We could be smarter here and normalise the path first, but '..'
           doesn't make sense for any of the current uses of [with_parent_dir]
           anyway. *)
        raise (Eio.Fs.err (Permission_denied (Err.Invalid_leaf leaf)))
      ) else (
        let dir = resolve t dir in
        Switch.run @@ fun sw ->
        let open Low_level in
        let dirfd = Err.run (Low_level.openat ~sw ~nofollow:true dir Flags.Open.(generic_read + synchronise) Flags.Disposition.(open_if)) Flags.Create.(directory) in
        fn (Some dirfd) leaf
      )
    ) else fn None path

  let v ~label ~sandbox dir_path = { dir_path; sandbox; label; closed = false }

  (* Sandboxes use [O_NOFOLLOW] when opening files ([resolve] already removed any symlinks).
     This avoids a race where symlink might be added after [realpath] returns. *)
  let opt_nofollow t = t.sandbox

  let open_in t ~sw path =
    let open Low_level in
    let fd = Err.run (Low_level.openat ~sw ~nofollow:(opt_nofollow t) (resolve t path)) Low_level.Flags.Open.(generic_read + synchronise) Flags.Disposition.(open_if) Flags.Create.(non_directory) in
    (Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

  let open_out t ~sw ~append ~create path =
    let open Low_level in
    let _mode, disp =
      match create with
      | `Never            -> 0,    Low_level.Flags.Disposition.open_
      | `If_missing  perm -> perm, Low_level.Flags.Disposition.open_if
      | `Or_truncate perm -> perm, Low_level.Flags.Disposition.overwrite_if
      | `Exclusive   perm -> perm, Low_level.Flags.Disposition.create
    in
    let flags =
      if append then Low_level.Flags.Open.(synchronise + append)
      else Low_level.Flags.Open.(generic_write + synchronise)
    in
    (* Bound the symlink redirections so a cyclic link reports ELOOP rather than
       recursing forever; 40 matches eio_posix and Linux's path_resolution(7). *)
    let rec loop follows_left path =
      match
        with_parent_dir t path @@ fun dirfd leaf ->
        begin match Low_level.openat ?dirfd ~nofollow:(opt_nofollow t) ~sw leaf flags disp Flags.Create.(non_directory) with
          | fd -> `Fd fd
          (* This is the result of raising [caml_unix_error(ELOOP,...)]: the leaf was
             a symlink. Read it relative to the parent [dirfd] (not the process
             working directory) so a sandboxed directory resolves the link
             correctly, then re-check the target is still in the sandbox. *)
          | exception Unix.Unix_error (EUNKNOWNERR 114, _, _) -> `Retry (Low_level.read_link ?dirfd leaf)
        end
      with
      | `Fd fd -> (Flow.of_fd fd :> Eio.File.rw_ty r)
      | `Retry _ when follows_left <= 0 -> raise (Err.wrap Unix.ELOOP "open_out" path)
      | `Retry target ->
        let full_target =
          if Filename.is_relative target then
            Filename.concat (Filename.dirname path) target
          else target
        in
        loop (follows_left - 1) full_target
      | exception Unix.Unix_error (code, name, arg) ->
        raise (Err.wrap code name arg)
    in
    loop 40 path

  let mkdir t ~perm path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.mkdir ?dirfd ~mode:perm) path

  let unlink t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:false) path

  let rmdir t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:true) path

  (* [resolve] prefixes sandbox paths with the \??\ NT namespace (needed by
     NtCreateFile); strip it back to a plain Win32 path for [Unix.lstat]. *)
  let strip_nt_prefix p =
    let pfx = "\\??\\" in
    let n = String.length pfx in
    if String.length p >= n && String.sub p 0 n = pfx
    then String.sub p n (String.length p - n)
    else p

  let stat t ~follow path =
    if follow then (
      (* Follow symlinks: [resolve] (realpath) already follows the whole path, so
         open it and fstat. [Create.empty] so it works on directories too. *)
      Switch.run @@ fun sw ->
      let open Low_level in
      let flags = Low_level.Flags.Open.(generic_read + synchronise) in
      let dis = Flags.Disposition.open_if in
      let fd = Err.run (openat ~sw ~nofollow:false (resolve t path) flags dis) Flags.Create.empty in
      Flow.Impl.stat fd
    ) else (
      (* Don't follow the leaf: resolve only the parent (sandbox-checked, follows
         the directory components) and [lstat] the leaf, so a symlink reports
         itself ([S_LNK]) rather than its target. *)
      let parent = strip_nt_prefix (resolve t (Filename.dirname path)) in
      let full = Filename.concat parent (Filename.basename path) in
      Low_level.eio_stat (Err.run Low_level.lstat full)
    )

  (* Open [path] as a directory handle (sandbox-checked and fully resolved by
     [resolve]) and enumerate it in a single pass. Working from the handle is
     race-free and returns each entry's kind alongside its name, so there is no
     per-entry [stat] (which would both cost N syscalls and race with concurrent
     modification, reporting removed entries as [`Unknown]). *)
  let list_dir t path =
    let open Low_level in
    Switch.run @@ fun sw ->
    let fd =
      Err.run (Low_level.openat ~sw ~nofollow:false (resolve t path)
                 Flags.Open.(generic_read + synchronise) Flags.Disposition.open_) Flags.Create.(directory)
    in
    Low_level.read_dir_entries fd

  let read_dir t path =
    list_dir t path |> List.map snd

  let with_dir_entries t path fn =
    fn (List.to_seq (list_dir t path))

  let read_link t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.read_link ?dirfd) path

  let chown ~follow ?uid ?gid t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (fun () -> Low_level.chown ?dirfd ~follow ?uid ?gid path) ()

  let rename t old_path new_dir new_path =
    match Handler.as_posix_dir new_dir with
    | None -> invalid_arg "Target is not an eio_windows directory!"
    | Some new_dir ->
      with_parent_dir t old_path @@ fun old_dir old_path ->
      with_parent_dir new_dir new_path @@ fun new_dir new_path ->
      Err.run (Low_level.rename ?old_dir old_path ?new_dir) new_path

  let symlink ~link_to t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.symlink ~link_to dirfd) path

  let close t = t.closed <- true

  let open_subtree t ~sw path =
    Switch.check sw;
    let label = Filename.basename path in
    let d = v ~label (resolve t path) ~sandbox:true in
    Switch.on_release sw (fun () -> close d);
    Eio.Resource.T (d, Handler.v)

  let chmod t ~follow ~perm path =
    (* [Low_level.chmod] with a dirfd is unsupported on Windows (no fchmodat), so
       resolve the leaf to an absolute path — sandbox-checked, like
       [stat ~follow:false] — and chmod that directly rather than passing a dirfd.
       Otherwise chmod through any sandboxed capability (incl. the default [cwd])
       would always raise EOPNOTSUPP. *)
    let parent = strip_nt_prefix (resolve t (Filename.dirname path)) in
    let full = Filename.concat parent (Filename.basename path) in
    Err.run (fun () ->
      (* [Unix.chmod] follows a symlink leaf, so with [~follow:false] reject a
         symlink target rather than changing the linked-to file's mode; chmod-ing
         the link itself is not meaningful on Windows. *)
      if not follow && (Low_level.lstat full).st_kind = Unix.S_LNK then
        raise (Unix.Unix_error (EOPNOTSUPP, "chmod", path));
      Low_level.chmod ~mode:perm None full) ()

  let pp f t = Fmt.string f (String.escaped t.label)

  let native_internal t path =
    if Filename.is_relative path then (
      let p =
        if t.dir_path = "." then path
        else Filename.concat t.dir_path path
      in
      if p = "" then "."
      else if p = "." then p
      else if Filename.is_implicit p then "./" ^ p
      else p
    ) else path

  let native t path =
    Some (native_internal t path)
end
and Handler : sig
  val v : (Dir.t, [`Dir | `Close]) Eio.Resource.handler

  val as_posix_dir : [> `Dir] r -> Dir.t option
end = struct
  (* When renaming, we get a plain [Eio.Fs.dir]. We need extra access to check
     that the new location is within its sandbox. *)
  type (_, _, _) Eio.Resource.pi += Posix_dir : ('t, 't -> Dir.t, [> `Posix_dir]) Eio.Resource.pi

  let as_posix_dir (Eio.Resource.T (t, ops)) =
    match Eio.Resource.get_opt ops Posix_dir with
    | None -> None
    | Some fn -> Some (fn t)

  let v = Eio.Resource.handler [
      H (Eio.Fs.Pi.Dir, (module Dir));
      H (Posix_dir, Fun.id);
    ]
end

(* Full access to the filesystem. *)
let fs = Eio.Resource.T (Dir.v ~label:"fs" ~sandbox:false ".", Handler.v)
let cwd = Eio.Resource.T (Dir.v ~label:"cwd" ~sandbox:true ".", Handler.v)
