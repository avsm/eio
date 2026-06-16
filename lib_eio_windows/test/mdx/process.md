# Process tests for the Windows backend

```ocaml
open Eio.Std

let run fn =
  Eio_windows.run @@ fun (env : Eio_unix.Stdenv.base) -> fn env#process_mgr

(* A command that exists on every Windows install. *)
let cmd args = "cmd.exe" :: "/c" :: args

(* Substring check, robust to the trailing CRLF that cmd.exe adds. *)
let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  go 0
```

## Capturing output

```ocaml
# run (fun mgr ->
    Eio.Process.parse_out mgr Eio.Buf_read.take_all (cmd ["echo"; "hello"]));;
- : string = "hello\r\n"
```

## Exit status

```ocaml
# run (fun mgr ->
    Switch.run @@ fun sw ->
    let child = Eio.Process.spawn ~sw mgr (cmd ["exit"; "3"]) in
    match Eio.Process.await child with
    | `Exited n -> Printf.sprintf "exited %d" n
    | `Signaled n -> Printf.sprintf "signaled %d" n);;
- : string = "exited 3"
```

A successful run passes `await_exn`:

```ocaml
# run (fun mgr ->
    Switch.run @@ fun sw ->
    Eio.Process.await_exn (Eio.Process.spawn ~sw mgr (cmd ["exit"; "0"])));;
- : unit = ()
```

## Passing the environment

```ocaml
# run (fun mgr ->
    Eio.Process.parse_out mgr Eio.Buf_read.take_all
      ~env:[| "EIO_TEST_VAR=from-eio" |] (cmd ["echo"; "%EIO_TEST_VAR%"]));;
- : string = "from-eio\r\n"
```

## Redirecting output to a flow without a file descriptor

A `~stdout` sink that has no underlying FD (here a buffer) makes `spawn` fork a
pipe and copy the child's output into it:

```ocaml
# run (fun mgr ->
    let buf = Buffer.create 16 in
    Eio.Process.run mgr ~stdout:(Eio.Flow.buffer_sink buf) (cmd ["echo"; "via-sink"]);
    contains ~needle:"via-sink" (Buffer.contents buf));;
- : bool = true
```

(The symmetric `~stdin` case — forwarding a non-FD source into the child — does
not work for typical console programs on Windows: the forwarded stdin is one end
of the AF_UNIX socketpair that emulates `Eio_unix.pipe`, and programs that
`ReadFile` their stdin reject a socket handle. A real overlapped pipe — part of
the IOCP migration — is needed for that.)

## Signalling an exited process is a no-op

```ocaml
# run (fun mgr ->
    Switch.run @@ fun sw ->
    let child = Eio.Process.spawn ~sw mgr (cmd ["exit"; "0"]) in
    Eio.Process.await_exn child;
    (* Terminating an already-exited process is ignored, not an error. *)
    Eio.Process.signal child Sys.sigkill;
    "ok");;
- : string = "ok"
```

## Releasing the switch stops the process

A long-running command that outlives its switch. `ping` starts instantly and
runs for well over a minute, so it is still alive when the switch is released.

```ocaml
let sleepy = ["ping"; "-n"; "100"; "-w"; "1000"; "127.0.0.1"]
```

Leaving the switch terminates the child. Awaiting the escaped process *after* the
switch has released must report the terminated exit code rather than crash on the
now-closed handle: the release hook reaps and caches the status before the handle
is closed. A Windows child killed via `TerminateProcess` reports `Exited 1`.

```ocaml
# run (fun mgr ->
    let child = Switch.run (fun sw -> Eio.Process.spawn ~sw mgr sleepy) in
    match Eio.Process.await child with
    | `Exited n -> Printf.sprintf "exited %d" n
    | `Signaled n -> Printf.sprintf "signaled %d" n);;
- : string = "exited 1"
```

Awaiting again returns the same cached status (the handle is long gone):

```ocaml
# run (fun mgr ->
    let child = Switch.run (fun sw -> Eio.Process.spawn ~sw mgr sleepy) in
    ignore (Eio.Process.await child : Eio.Process.exit_status);
    match Eio.Process.await child with
    | `Exited n -> Printf.sprintf "exited %d" n
    | `Signaled n -> Printf.sprintf "signaled %d" n);;
- : string = "exited 1"
```

Explicitly killing the process before leaving the switch also reports `Exited 1`,
and the subsequent release is a harmless no-op:

```ocaml
# run (fun mgr ->
    let child = Switch.run (fun sw ->
      let child = Eio.Process.spawn ~sw mgr sleepy in
      Eio.Process.signal child Sys.sigkill;
      child) in
    match Eio.Process.await child with
    | `Exited n -> Printf.sprintf "exited %d" n
    | `Signaled n -> Printf.sprintf "signaled %d" n);;
- : string = "exited 1"
```
