# Pseudoterminal tests for the Windows backend

These exercise `Eio_windows.Pty` (ConPTY). The child's output on the master is
VT/ANSI-encoded, so the tests scan the accumulated bytes for a substring rather
than comparing exact output.

```ocaml
open Eio.Std
module Pty = Eio_windows.Pty

let run fn = Eio_windows.run @@ fun (env : Eio_unix.Stdenv.base) -> fn env

(* The portable [spawn_unix]'s PATH resolution is POSIX-only, so give it the
   command interpreter's absolute path directly. *)
let comspec = try Sys.getenv "COMSPEC" with Not_found -> {|C:\Windows\System32\cmd.exe|}

(* Substring search over the accumulated (VT-noisy) master output. *)
let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  go 0

(* Read chunks from [src] until [needle] appears in the accumulated bytes (or a
   defensive [max_reads] cap is hit). Each read blocks only while the console is
   still producing output; we stop the instant the needle is found, so we never
   issue the read-with-no-data that would block after the child is done — the
   pseudoconsole keeps the master open past child exit, so there is no EOF to
   wait on. *)
let read_until ~needle ~max_reads src =
  let buf = Cstruct.create 4096 in
  let acc = Buffer.create 256 in
  let rec loop n =
    if contains ~needle (Buffer.contents acc) then Buffer.contents acc
    else if n >= max_reads then Buffer.contents acc
    else
      match Eio.Flow.single_read src buf with
      | got ->
        Buffer.add_string acc (Cstruct.to_string (Cstruct.sub buf 0 got));
        loop (n + 1)
      | exception End_of_file -> Buffer.contents acc
  in
  loop 0
```

## Open, resize and close lifecycle

The default size is 80×24; `resize` updates the stored size, and `name` is a
non-empty synthetic identifier (the backing pipes are anonymous).

```ocaml
# run (fun _ ->
    Switch.run @@ fun sw ->
    let pty = Pty.open_pty ~sw () in
    let d = Pty.get_window_size pty in
    Pty.resize pty { d with Pty.rows = 30; cols = 100 };
    let r = Pty.get_window_size pty in
    (String.length (Pty.name pty) > 0, d.Pty.rows, d.Pty.cols, r.Pty.rows, r.Pty.cols));;
- : bool * int * int * int * int = (true, 24, 80, 30, 100)
```

## Spawning a child attached to the pty

`cmd /c echo hello` is attached via `~login_tty`; its echoed output reaches the
master (surrounded by VT sequences), and the child exits with status 0.

```ocaml
# run (fun (env : Eio_unix.Stdenv.base) ->
    Switch.run @@ fun sw ->
    let pty = Pty.open_pty ~sw () in
    let child =
      Eio_unix.Process.spawn_unix ~sw env#process_mgr
        ~login_tty:(Pty.tty pty) ~fds:[] ~executable:comspec
        ["cmd.exe"; "/c"; "echo"; "hello"]
    in
    let out = read_until ~needle:"hello" ~max_reads:64 (Pty.source pty) in
    Eio.Process.await_exn child;
    contains ~needle:"hello" out);;
- : bool = true
```

## A non-pty login_tty fd is rejected

An fd that is not the tty token of a pty (here the pty's own master end) cannot
be a controlling terminal, so `spawn_unix` rejects it.

```ocaml
# run (fun (env : Eio_unix.Stdenv.base) ->
    Switch.run @@ fun sw ->
    let pty = Pty.open_pty ~sw () in
    match
      Eio_unix.Process.spawn_unix ~sw env#process_mgr
        ~login_tty:(Pty.pty pty) ~fds:[] ~executable:comspec
        ["cmd.exe"; "/c"; "echo"; "x"]
    with
    | _ -> "no error"
    | exception Invalid_argument _ -> "rejected");;
- : string = "rejected"
```
