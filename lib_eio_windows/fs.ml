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

(* See fs.mli for how Windows paths and sandboxes work. *)

open Eio.Std

module Fd = Eio_unix.Fd
module Nt_path = Eio_utils.Nt_path

(* Opening paths beneath a sandbox directory [dir].

   Each open is relative to a handle on [dir] with [~beneath:true], so the kernel
   refuses to follow any link and ".." can't be used. If that fails because of a
   link, we find the link, replace it with its target, and try again. *)
module Sandbox = struct
  let max_follows = 40          (* As on Linux; see path_resolution(7) *)

  (* The components of [path] below [root], where [root] is (the real path of) [dir]. *)
  let components ~dir ~root path =
    match Nt_path.beneath ~root path with
    | Some c -> c
    | None -> raise @@ Eio.Fs.err (Permission_denied (Err.Outside_sandbox (path, dir)))

  (* [join base comps] is [base] followed by [comps]. *)
  let join base = function
    | [] -> base
    | comps -> Nt_path.join base (String.concat "\\" comps)

  let open_at root ~sw ~follow comps =
    Low_level.openat ~sw ~dirfd:root ~beneath:true ~follow (join "" comps)

  (* The target of [comps], if it is a link. *)
  let link_target root comps =
    Switch.run @@ fun sw ->
    let open Low_level in
    read_link_fd (open_at root ~sw ~follow:Open_link comps Flags.Open.synchronise Flags.Disposition.open_ Flags.Create.empty)

  (* The number of components up to and including the first link in [comps], and its target.
     A link at the leaf doesn't count if it is to be opened itself. *)
  let find_link root ~follow comps =
    let last = List.length comps - (if follow = Low_level.Open_link then 1 else 0) in
    let rec go n =
      if n > last then None
      else match link_target root (List.take n comps) with
        | Some target -> Some (n, target)
        | None -> go (n + 1)
    in
    go 1

  (* Open [comps] beneath [dir], returning the FD and a Win32 path for it.
     Windows won't rename an open object or any directory above it, so the path names
     the same object while the FD is open, and can be given to OCaml's [Unix] functions.
     [follow] says what to do with a link at the leaf. *)
  let open_ ~dir ~sw ~follow comps flags disp create =
    let root_path = Low_level.realpath dir in
    Switch.run @@ fun tmp ->
    let root = Low_level.(openat ~sw:tmp root_path Flags.Open.(generic_read + synchronise) Flags.Disposition.open_ Flags.Create.directory) in
    let rec go follows comps =
      match open_at root ~sw ~follow comps flags disp create with
      | fd -> fd, join root_path comps
      | exception (Unix.Unix_error _ as ex) ->
        (* Usually a link gives [ELOOP], but a link at the leaf can fail first if it's
           to a file and we wanted a directory, or the other way around. *)
        match find_link root ~follow comps with
        | Some (n, _) when n = List.length comps && follow = Low_level.Nofollow ->
          raise (Unix.Unix_error (ELOOP, "openat", join "" comps))
        | Some (n, target) when follows > 0 ->
          (* A relative target is from the link's directory, and an absolute one replaces it. *)
          let parent = join "" (List.take (n - 1) comps) and rest = List.drop n comps in
          go (follows - 1) (components ~dir ~root:root_path (join (Nt_path.join parent target) rest))
        | _ -> raise ex
    in
    go max_follows comps
end

module rec Dir : sig
  include Eio.Fs.Pi.DIR
  val v : label:string -> sandbox:bool -> string -> t
  val resolve : t -> string -> string
  val with_parent_dir : t -> string -> (Fd.t option -> string -> 'a) -> 'a
end = struct
  type t = {
    dir_path : string;
    sandbox : bool;
    label : string;
    mutable closed : bool;
  }

  (* The components of [path], which must be relative. *)
  let relative t path =
    if not (Nt_path.is_relative path) then raise @@ Eio.Fs.err (Permission_denied Err.Absolute_path);
    Sandbox.components ~dir:t.dir_path ~root:t.dir_path path

  (* The components of the parent of [path], and its leaf. *)
  let parent_and_leaf t path =
    match List.rev (relative t path) with
    | [] -> raise (Eio.Fs.err (Permission_denied (Err.Invalid_leaf path)))
    | leaf :: parent -> List.rev parent, leaf

  let open_beneath t ~sw ~follow comps flags disp create =
    if t.closed then Fmt.invalid_arg "Attempt to use closed directory %S" t.dir_path;
    Sandbox.open_ ~dir:t.dir_path ~sw ~follow comps flags disp create

  let open_path t ~sw ~follow path flags disp create =
    Err.run (fun () ->
        if t.sandbox then fst (open_beneath t ~sw ~follow (relative t path) flags disp create)
        else Low_level.openat ~sw ~follow path flags disp create
      ) ()

  (* Run [fn p], where [p] is a Win32 path for [comps] that is valid during [fn]. *)
  let with_path t ~follow comps create fn =
    Switch.run @@ fun sw ->
    let open Low_level in
    let _fd, path =
      Err.run (fun () -> open_beneath t ~sw ~follow comps Flags.Open.synchronise Flags.Disposition.open_ create) ()
    in
    fn path

  let resolve t path =
    if t.sandbox then with_path t ~follow:Follow (relative t path) Low_level.Flags.Create.empty Fun.id
    else path

  let with_parent_dir t path fn =
    if t.sandbox then (
      let parent, leaf = parent_and_leaf t path in
      Switch.run @@ fun sw ->
      let open Low_level in
      let dirfd, _ =
        Err.run (fun () ->
            open_beneath t ~sw ~follow:Follow parent
              Flags.Open.(generic_read + synchronise) Flags.Disposition.open_ Flags.Create.directory
          ) ()
      in
      fn (Some dirfd) leaf
    ) else fn None path

  let v ~label ~sandbox dir_path = { dir_path; sandbox; label; closed = false }

  let open_in t ~sw ~follow path =
    let fd =
      open_path t ~sw path
        ~follow:(if follow then Low_level.Follow else Nofollow)
        Low_level.Flags.Open.(generic_read + synchronise)
        Low_level.Flags.Disposition.open_
        Low_level.Flags.Create.non_directory
    in
    (Flow.of_fd fd :> Eio.File.ro_ty Eio.Resource.t)

  let open_out t ~sw ~follow ~append ~create path =
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
    let fd =
      open_path t ~sw path flags disp Low_level.Flags.Create.non_directory
        ~follow:(if follow then Low_level.Follow else Nofollow)
    in
    (Flow.of_fd fd :> Eio.File.rw_ty r)

  let mkdir t ~perm path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.mkdir ?dirfd ~mode:perm) path

  let unlink t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (fun () ->
        try Low_level.unlink ?dirfd ~dir:false path
        with Unix.Unix_error _ as ex ->
          (* A link to a directory is itself a directory on Windows *)
          try ignore (Low_level.read_link ?dirfd path : string); Low_level.unlink ?dirfd ~dir:true path
          with Unix.Unix_error _ -> raise ex
      ) ()

  let rmdir t path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.unlink ?dirfd ~dir:true) path

  let stat t ~follow path =
    Switch.run @@ fun sw ->
    let fd =
      open_path t ~sw path
        ~follow:(if follow then Low_level.Follow else Open_link)
        Low_level.Flags.Open.(generic_read + synchronise)
        Low_level.Flags.Disposition.open_
        Low_level.Flags.Create.empty
    in
    Flow.Impl.stat fd

  let read_dir t path =
    let read path = Err.run Low_level.readdir path |> Array.to_list in
    if t.sandbox then with_path t ~follow:Follow (relative t path) Low_level.Flags.Create.directory read
    else read path

  let with_dir_entries t path fn =
    let entries =
      read_dir t path
      |> List.map (fun name ->
          match stat ~follow:false t (Nt_path.join path name) with
          | info -> (info.kind, name)
          | exception Eio.Exn.Io _ -> (`Unknown, name)
        )
    in
    fn (List.to_seq entries)

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

  (* Windows links are either to files or to directories,
     so pick by what [link_to] is now, as [Unix.symlink] does. *)
  let symlink ~link_to t path =
    let to_dir =
      let target =
        if Nt_path.is_relative link_to then Nt_path.join (Nt_path.dirname path) link_to
        else link_to
      in
      match (stat t ~follow:true target).kind with
      | `Directory -> true
      | _ | exception Eio.Io _ -> false
    in
    let create path = Err.run (Low_level.symlink ~to_dir ~link_to None) path in
    if t.sandbox then (
      let parent, leaf = parent_and_leaf t path in
      with_path t ~follow:Follow parent Low_level.Flags.Create.directory @@ fun parent ->
      create (Nt_path.join parent leaf)
    ) else create path

  let close t = t.closed <- true

  let open_subtree t ~sw path =
    Switch.check sw;
    let label = Nt_path.basename path in
    let d = v ~label (resolve t path) ~sandbox:true in
    Switch.on_release sw (fun () -> close d);
    Eio.Resource.T (d, Handler.v)

  let chmod t ~follow:_ ~perm path =
    with_parent_dir t path @@ fun dirfd path ->
    Err.run (Low_level.chmod ~mode:perm dirfd) path

  let sync_dir _t _path = ()        (* Directories do not require fsync on Windows *)

  let pp f t = Fmt.string f (String.escaped t.label)

  let native_internal t path =
    if Nt_path.is_relative path then (
      let p =
        if t.dir_path = "." then path
        else Nt_path.join t.dir_path path
      in
      if p = "" then "."
      else if p = "." then p
      else if Filename.is_implicit p then ".\\" ^ p
      else p
    ) else path

  let native t path =
    Some (native_internal t path)

  include Eio_utils.Nt_path
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

let dir ~label ~sandbox path = Eio.Resource.T (Dir.v ~label ~sandbox path, Handler.v)

let fs = Eio.Path.of_dir (dir ~label:"fs" ~sandbox:false ".")
let cwd = Eio.Path.of_dir (dir ~label:"cwd" ~sandbox:true ".")
