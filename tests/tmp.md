# Temporary files and directories

```ocaml
# #require "eio_main";;
# ignore @@ Unix.umask 0o022;;
- : unit = ()
```

```ocaml
open Eio.Std

let ( / ) = Eio.Path.( / )

(* Run with a fresh, empty directory ["test-tmp"] under cwd, used as the parent
   for temporary entries so the tests are isolated from the real system temp
   directory. *)
let run fn =
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  let root = cwd / "test-tmp" in
  Eio.Path.rmtree ~missing_ok:true root;
  Eio.Path.mkdir ~perm:0o700 root;
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
    (fun () -> fn root)

let count dir = List.length (Eio.Path.read_dir dir)
```

## Temporary directories

A temporary directory is created and removed again when the function returns:

```ocaml
# run (fun root ->
    Eio.Path.with_tmp_dir root (fun dir ->
      Eio.Path.save ~create:(`Exclusive 0o600) (dir / "data") "scratch";
      traceln "while open: %d entry, contents %S" (count root) (Eio.Path.load (dir / "data")));
    traceln "after close: %d entries" (count root));;
+while open: 1 entry, contents "scratch"
+after close: 0 entries
- : unit = ()
```

It is removed even if the function raises:

```ocaml
# run (fun root ->
    begin try
      Eio.Path.with_tmp_dir root (fun _dir -> failwith "boom")
    with Failure msg -> traceln "caught %S" msg end;
    traceln "after exception: %d entries" (count root));;
+caught "boom"
+after exception: 0 entries
- : unit = ()
```

With [~cleanup:false] it is left in place:

```ocaml
# run (fun root ->
    Eio.Path.with_tmp_dir ~cleanup:false root (fun _dir -> ());
    traceln "kept: %d entry" (count root));;
+kept: 1 entry
- : unit = ()
```

## Temporary files

A temporary file is anonymous: it is not linked into the directory (so it never
appears in [read_dir]), and its storage is reclaimed when the flow is closed:

```ocaml
# run (fun root ->
    Eio.Path.with_tmp_file root (fun f ->
      Eio.Flow.copy_string "hello" f;
      traceln "while open: %d entries" (count root));
    traceln "after close: %d entries" (count root));;
+while open: 0 entries
+after close: 0 entries
- : unit = ()
```

## Atomic writes

A successful atomic write leaves the data at the destination and no temporary
files behind:

```ocaml
# run (fun root ->
    let dst = root / "out" in
    Eio.Path.with_open_out ~atomic:true ~create:(`Or_truncate 0o644) dst (fun f ->
      Eio.Flow.copy_string "complete" f);
    traceln "entries: %d, contents: %S" (count root) (Eio.Path.load dst));;
+entries: 1, contents: "complete"
- : unit = ()
```

If the writer raises, the destination is untouched and no temporary file is left:

```ocaml
# run (fun root ->
    let dst = root / "out" in
    Eio.Path.save ~create:(`Exclusive 0o600) dst "original";
    begin try
      Eio.Path.with_open_out ~atomic:true ~create:(`Or_truncate 0o644) dst (fun f ->
        Eio.Flow.copy_string "partial" f;
        failwith "boom")
    with Failure _ -> () end;
    traceln "entries: %d, contents: %S" (count root) (Eio.Path.load dst));;
+entries: 1, contents: "original"
- : unit = ()
```

`Exclusive` refuses to replace an existing file:

```ocaml
# run (fun root ->
    let dst = root / "out" in
    Eio.Path.save ~create:(`Exclusive 0o600) dst "original";
    match Eio.Path.save ~atomic:true ~create:(`Exclusive 0o644) dst "new" with
    | () -> traceln "replaced"
    | exception Eio.Io _ -> traceln "refused; contents still %S" (Eio.Path.load dst));;
+refused; contents still "original"
- : unit = ()
```

`~append` and `~atomic` cannot be combined:

```ocaml
# run (fun root ->
    let dst = root / "out" in
    match Eio.Path.save ~append:true ~atomic:true ~create:(`Or_truncate 0o644) dst "x" with
    | () -> traceln "ok"
    | exception Invalid_argument msg -> traceln "rejected: %s" msg);;
+rejected: Eio.Path.with_open_out: ~append and ~atomic are incompatible
- : unit = ()
```

## The system temporary directory

To create temporary entries in the system temporary directory, combine
`Eio.Stdenv.fs` with `Filename.get_temp_dir_name`:

```ocaml
# Eio_main.run (fun env ->
    let tmp = Eio.Stdenv.fs env / Filename.get_temp_dir_name () in
    Eio.Path.with_tmp_dir tmp (fun dir ->
      Eio.Path.save ~create:(`Exclusive 0o600) (dir / "data") "scratch";
      traceln "read back %S" (Eio.Path.load (dir / "data"))));;
+read back "scratch"
- : unit = ()
```
