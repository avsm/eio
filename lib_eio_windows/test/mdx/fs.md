# Filesystem tests for the Windows backend

Modules come from the dune `(libraries ...)` field, so no `#require` is needed.

```ocaml
module Path = Eio.Path
open Eio.Std

let ( / ) = Path.( / )

(* Each test runs in its own fresh, sandboxed subtree, so the suite is isolated
   and repeatable regardless of the real working directory. Operating on the
   subtree root (rather than a sub-path of it) mirrors how the cwd behaves. The
   leading "_" also stops dune from scanning it as a source directory. *)
let run fn =
  Eio_windows.run @@ fun (env : Eio_unix.Stdenv.base) ->
  let tmp = Eio.Stdenv.cwd env / "_mdx_tmp" in
  Path.rmtree ~missing_ok:true tmp;
  Path.mkdir ~perm:0o700 tmp;
  Fun.protect (fun () -> Path.with_subtree tmp fn)
    ~finally:(fun () -> Path.rmtree ~missing_ok:true tmp)
```

## Create, write and read back

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "f") "my-data";
    Path.load (dir / "f"));;
- : string = "my-data"
```

Creating it again exclusively fails:

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "f") "one";
    try Path.save ~create:(`Exclusive 0o600) (dir / "f") "two"; "no error"
    with Eio.Io (Eio.Fs.E (Already_exists _), _) -> "already-exists");;
- : string = "already-exists"
```

Truncating replaces the contents; appending adds to them:

```ocaml
# run (fun dir ->
    Path.save ~create:(`Or_truncate 0o600) (dir / "f") "fresh";
    Path.save ~create:`Never ~append:true (dir / "f") "-more";
    Path.load (dir / "f"));;
- : string = "fresh-more"
```

`If_missing` creates the file, but on an existing file it writes from the start
without truncating (so the tail of the longer original is left in place):

```ocaml
# run (fun dir ->
    Path.save ~create:(`If_missing 0o600) (dir / "f") "1st-write-original";
    Path.save ~create:(`If_missing 0o600) (dir / "f") "2nd-write";
    Path.load (dir / "f"));;
- : string = "2nd-write-original"
```

Writing to a missing file with `Never` fails:

```ocaml
# run (fun dir ->
    try Path.save ~create:`Never (dir / "missing") "data"; "no-error"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> "not-found");;
- : string = "not-found"
```

## Directories

`mkdir` then `stat` — exercises stat on a directory (it used to fail):

```ocaml
# run (fun dir ->
    Path.mkdir ~perm:0o700 (dir / "d");
    (Path.is_directory (dir / "d"), Path.is_file (dir / "d")));;
- : bool * bool = (true, false)
```

`mkdirs` creates intermediate directories; repeating fails unless `~exists_ok`:

```ocaml
# run (fun dir ->
    let nested = dir / "a" / "b" / "c" in
    Path.mkdirs ~perm:0o700 nested;
    let again () =
      try Path.mkdirs ~perm:0o700 nested; "ok"
      with Eio.Io (Eio.Fs.E (Already_exists _), _) -> "already-exists" in
    let r1 = again () in
    Path.mkdirs ~exists_ok:true ~perm:0o700 nested;
    (Path.is_directory nested, r1));;
- : bool * string = (true, "already-exists")
```

Opening a directory as a regular file is rejected at open time: the sandbox
keeps its `FILE_NON_DIRECTORY_FILE` constraint even under `~nofollow`, so
`with_open_in` raises rather than handing back a directory handle:

```ocaml
# run (fun dir ->
    Path.mkdir ~perm:0o700 (dir / "d");
    match Path.with_open_in (dir / "d") (fun f -> Fmt.str "%a" Eio.File.Stat.pp_kind (Eio.File.stat f).kind) with
    | kind -> "opened as " ^ kind
    | exception (Eio.Io _ | Unix.Unix_error _) -> "rejected");;
- : string = "rejected"
```

The native by-handle `fstat` fills in `blocks` and `blksize` (both zero when
Windows stats went through `Unix.LargeFile.fstat`), and reports a positive size
and a `Regular_file` kind. The payload is large enough to be stored non-resident
(a tiny file lives inside the MFT record and reports a zero allocation):

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "f") (String.make 65536 'x');
    Path.with_open_in (dir / "f") (fun f ->
      let s = Eio.File.stat f in
      (s.kind, Optint.Int63.to_int s.size > 0, s.blksize > 0L, s.blocks > 0L)));;
- : Eio.File.Stat.kind * bool * bool * bool =
(`Regular_file, true, true, true)
```

## Reading and removing directories

```ocaml
# run (fun dir ->
    Path.mkdir ~perm:0o700 (dir / "d");
    Path.save ~create:(`Exclusive 0o600) (dir / "d" / "f") "x";
    let entries = Path.read_dir (dir / "d") in
    Path.unlink (dir / "d" / "f");
    Path.rmdir (dir / "d");
    (entries, Path.is_directory (dir / "d")));;
- : string list * bool = (["f"], false)
```

`read_dir_entries` returns each entry's kind alongside its name in a single pass
(no per-entry `stat`), classifying a regular file and a subdirectory directly
from the directory listing:

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "f") "x";
    Path.mkdir ~perm:0o700 (dir / "sub");
    Path.read_dir_entries dir
    |> List.map (fun (k, n) -> (n, Fmt.str "%a" Eio.File.Stat.pp_kind k)));;
- : (string * string) list = [("f", "regular file"); ("sub", "directory")]
```

A symlink is reported as `Symbolic_link` (distinguished from other reparse
points by its reparse tag), where symlink creation is permitted:

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "target") "x";
    match Path.symlink ~link_to:"target" (dir / "l") with
    | exception Eio.Io _ -> "symlinks-unsupported"
    | () ->
      match List.assoc_opt "l"
              (List.map (fun (k, n) -> (n, k)) (Path.read_dir_entries dir)) with
      | Some `Symbolic_link -> "symlink"
      | Some _ -> "other-kind"
      | None -> "missing");;
- : string = "symlink"
```

Non-ASCII entry names round-trip through the UTF-16 listing unchanged:

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "café-\xe6\x97\xa5") "x";
    Path.read_dir dir);;
- : string list = ["café-日"]
```

## Renaming

```ocaml
# run (fun dir ->
    Path.save ~create:(`Or_truncate 0o600) (dir / "a") "renamed-data";
    Path.rename (dir / "a") (dir / "b");
    (Path.load (dir / "b"), Path.is_file (dir / "a")));;
- : string * bool = ("renamed-data", false)
```

Renaming also moves a file between directories:

```ocaml
# run (fun dir ->
    Path.mkdir ~perm:0o700 (dir / "d1");
    Path.mkdir ~perm:0o700 (dir / "d2");
    Path.save ~create:(`Or_truncate 0o600) (dir / "d1" / "a") "moved";
    Path.rename (dir / "d1" / "a") (dir / "d2" / "b");
    (Path.load (dir / "d2" / "b"), Path.is_file (dir / "d1" / "a")));;
- : string * bool = ("moved", false)
```

Renaming onto an existing file replaces it (the stub sets `ReplaceIfExists`):

```ocaml
# run (fun dir ->
    Path.save ~create:(`Or_truncate 0o600) (dir / "a") "new";
    Path.save ~create:(`Or_truncate 0o600) (dir / "b") "old";
    Path.rename (dir / "a") (dir / "b");
    (Path.load (dir / "b"), Path.is_file (dir / "a")));;
- : string * bool = ("new", false)
```

## Unlinking

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "f") "data";
    Path.unlink (dir / "f");
    Path.is_file (dir / "f"));;
- : bool = false
```

Unlinking something that isn't there fails:

```ocaml
# run (fun dir ->
    try Path.unlink (dir / "missing"); "no-error"
    with Eio.Io (Eio.Fs.E (Not_found _), _) -> "not-found");;
- : string = "not-found"
```

## The cwd sandbox rejects absolute paths

A directory resource only grants access below itself, so an absolute path is
refused:

```ocaml
# Eio_windows.run (fun env ->
    let cwd = Eio.Stdenv.cwd env in
    let abs = Filename.temp_file "eio" "win" in
    Fun.protect ~finally:(fun () -> Sys.remove abs) @@ fun () ->
    try Path.save ~create:(`Exclusive 0o600) (cwd / abs) "data"; "no-error"
    with Eio.Io (Eio.Fs.E (Permission_denied _), _) -> "permission-denied");;
- : string = "permission-denied"
```

## Positioned I/O (preadv / pwritev)

```ocaml
# run (fun dir ->
    Path.with_open_out ~create:(`Or_truncate 0o600) (dir / "p") (fun f ->
      ignore (Eio.File.pwrite_single f ~file_offset:(Optint.Int63.of_int 0)
                [Cstruct.of_string "HELLO-WORLD"] : int));
    Path.with_open_in (dir / "p") (fun f ->
      let buf = Cstruct.create 5 in
      Eio.File.pread_exact f ~file_offset:(Optint.Int63.of_int 6) [buf];
      Cstruct.to_string buf));;
- : string = "WORLD"
```

## Seek

`seek` moves the file position: seeking to offset 3 from the start reports 3,
and reading from there returns the remainder. `seek 0 SEEK_CUR` then reports the
position advanced past the read (6). It used to return garbage because
`Unix.LargeFile.lseek` misbehaves on the backend's raw file handles, so the
backend now seeks natively with `SetFilePointerEx`.

```ocaml
# run (fun dir ->
    Path.save ~create:(`Or_truncate 0o600) (dir / "s") "abc123";
    Path.with_open_in (dir / "s") (fun f ->
      let from_start = Eio.File.seek f (Optint.Int63.of_int 3) `Set in
      let buf = Cstruct.create 16 in
      let n = Eio.Flow.single_read f buf in
      let rest = Cstruct.to_string (Cstruct.sub buf 0 n) in
      let cur = Eio.File.seek f (Optint.Int63.of_int 0) `Cur in
      (Optint.Int63.to_int from_start, rest, Optint.Int63.to_int cur)));;
- : int * string * int = (3, "123", 6)
```

## Symlinks

Creating symlinks needs Developer Mode or the symlink privilege; otherwise this
reports `EOPNOTSUPP` instead of crashing.

```ocaml
# run (fun dir ->
    Path.save ~create:(`Or_truncate 0o600) (dir / "target") "via-link";
    match Path.symlink ~link_to:"target" (dir / "link") with
    | () -> Path.load (dir / "link")
    | exception Eio.Io _ -> "symlinks-unsupported");;
- : string = "via-link"
```

`chmod ~follow:false` changes a regular file's mode, but refuses to act through
a symlink leaf (which would otherwise silently change the target's mode):

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "target") "data";
    let on_file =
      match Eio.Path.chmod ~follow:false ~perm:0o600 (dir / "target") with
      | () -> "ok" | exception _ -> "failed" in
    let on_link =
      match Path.symlink ~link_to:"target" (dir / "link") with
      | exception Eio.Io _ -> "symlinks-unsupported"
      | () ->
        match Eio.Path.chmod ~follow:false ~perm:0o600 (dir / "link") with
        | () -> "followed-link"
        | exception Eio.Io _ -> "rejected" in
    (on_file, on_link));;
- : string * string = ("ok", "rejected")
```

## Case-insensitive, but case-preserving (unlike POSIX)

Windows matches filenames case-insensitively, so a file created in one case can
be opened in another (on a typical POSIX filesystem this would be `Not_found`):

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "Foo") "data";
    Path.load (dir / "foo"));;
- : string = "data"
```

The original case is still preserved in the directory listing:

```ocaml
# run (fun dir ->
    Path.save ~create:(`Exclusive 0o600) (dir / "MixedCase.TXT") "x";
    Path.read_dir dir);;
- : string list = ["MixedCase.TXT"]
```

## Backslash is a path separator (unlike POSIX)

On Windows `\` separates path components, where on POSIX it is an ordinary
filename character. So a leaf containing a backslash addresses a nested file,
reachable equally by the portable forward-slash form:

```ocaml
# run (fun dir ->
    Path.mkdir ~perm:0o700 (dir / "sub");
    Path.save ~create:(`Exclusive 0o600) (dir / {|sub\leaf|}) "via-backslash";
    Path.load (dir / "sub" / "leaf"));;
- : string = "via-backslash"
```

## A Unix-only block, skipped on Windows

The `$MDX` annotation conditions the block so it only runs off Windows (here it
is skipped, so `Unix.umask` — unimplemented on Windows — is never reached):

<!-- $MDX os_type<>Win32 -->
```ocaml
# ignore @@ Unix.umask 0o022;;
- : unit = ()
```
