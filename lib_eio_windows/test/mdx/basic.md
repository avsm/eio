# Eio_windows mdx smoke test

Modules are provided by the dune `(libraries ...)` field, so no `#require` is
needed (which also keeps findlib from printing load messages).

```ocaml
open Eio.Std

let run fn =
  Eio_windows.run @@ fun (env : Eio_unix.Stdenv.base) ->
  fn ~clock:env#clock
```

A trivial computation inside the event loop:

```ocaml
# Eio_windows.run (fun _env -> 1 + 1);;
- : int = 2
```

A clock sleep actually waits:

```ocaml
# run @@ fun ~clock ->
    let t0 = Unix.gettimeofday () in
    Eio.Time.sleep clock 0.01;
    Unix.gettimeofday () -. t0 >= 0.01;;
- : bool = true
```

This block only runs off Windows, so it is skipped here:

<!-- $MDX os_type<>Win32 -->
```ocaml
# print_endline "should not run on windows";;
should not run on windows
- : unit = ()
```
