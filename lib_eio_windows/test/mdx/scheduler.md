# Scheduler tests for the Windows backend

```ocaml
open Eio.Std

let run fn = Eio_windows.run @@ fun _env -> fn ()
```

## Domain-local-await works across domains

A foreign domain waits (via `kcas`) for the main fibre to set `x`, then sets `y`;
the main fibre waits for `y`. This exercises the domain-local-await hook the
scheduler installs:

```ocaml
# run (fun () ->
    let open Kcas in
    let x = Loc.make 0 and y = Loc.make 0 in
    let foreign = Domain.spawn (fun () ->
      let x = Loc.get_as (fun x -> Retry.unless (x <> 0); x) x in
      Loc.set y 22;
      x)
    in
    Loc.set x 20;
    let y' = Loc.get_as (fun y -> Retry.unless (y <> 0); y) y in
    y' + Domain.join foreign);;
- : int = 42
```

## Cancelling an FD await doesn't strand the FD

Start awaiting readable/writable, cancel immediately, then await on fresh FDs —
this fails if the cancelled FDs were left registered with the scheduler:

```ocaml
# run (fun () ->
    let a, b = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
    (try
       Eio.Cancel.sub (fun cc ->
         Fiber.all [
           (fun () -> Eio_unix.await_readable a);
           (fun () -> Eio_unix.await_writable b);
           (fun () -> Eio.Cancel.cancel cc Exit);
         ];
         assert false)
     with Eio.Cancel.Cancelled _ -> ());
    let c, d = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
    Unix.close a;
    Unix.close b;
    Fiber.first
      (fun () -> Eio_unix.await_readable c)
      (fun () -> Eio_unix.await_writable d);
    Unix.close c;
    Unix.close d;
    "ok");;
- : string = "ok"
```
