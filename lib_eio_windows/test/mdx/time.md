# Clock and randomness tests for the Windows backend

```ocaml
open Eio.Std

let run fn = Eio_windows.run @@ fun (env : Eio_unix.Stdenv.base) -> fn env
```

## Sleeping actually waits

```ocaml
# run (fun env ->
    let t0 = Unix.gettimeofday () in
    Eio.Time.sleep env#clock 0.01;
    Unix.gettimeofday () -. t0 >= 0.01);;
- : bool = true
```

## A short sleep loses the race and is cancelled

```ocaml
# run (fun env ->
    Fiber.first
      (fun () -> Eio.Time.sleep env#clock 60.; "slow")
      (fun () -> "fast"));;
- : string = "fast"
```

## Secure randomness returns different bytes each read

```ocaml
# run (fun env ->
    let src = env#secure_random in
    let b1 = Cstruct.create 8 and b2 = Cstruct.create 8 in
    Eio.Flow.read_exact src b1;
    Eio.Flow.read_exact src b2;
    not (Cstruct.equal b1 b2));;
- : bool = true
```
