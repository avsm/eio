# Networking tests for the Windows backend

```ocaml
open Eio.Std

let run fn = Eio_windows.run @@ fun (env : Eio_unix.Stdenv.base) -> fn env

let read_all flow = Eio.Buf_read.(of_flow flow ~max_size:100 |> take_all)
```

```ocaml
# #install_printer Eio.Net.Ipaddr.pp;;
```

## Socket pair

A connected pair of stream sockets, no addresses needed:

```ocaml
# run (fun _env ->
    Switch.run @@ fun sw ->
    let a, b = Eio_unix.Net.socketpair_stream ~sw () in
    Eio.Flow.copy_string "ping" a;
    Eio.Flow.close a;
    read_all b);;
- : string = "ping"
```

## Pipes

`Eio_unix.pipe` is emulated with a socketpair on Windows (anonymous pipes can't
be made non-blocking), but the read end still presents a clean end-of-file once
the write end is closed — the backend maps the socketpair's `ECONNRESET` to EOF:

```ocaml
# run (fun _env ->
    Switch.run @@ fun sw ->
    let r, w = Eio_unix.pipe sw in
    Eio.Flow.copy_string "piped-data" w;
    Eio.Flow.close w;
    read_all r);;
- : string = "piped-data"
```

## TCP client / server over loopback

```ocaml
# run (fun env ->
    Switch.run @@ fun sw ->
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9731) in
    let server = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:5 addr in
    let received = ref "" in
    Fiber.both
      (fun () ->
        let flow, _addr = Eio.Net.accept ~sw server in
        Eio.Flow.copy_string "hello-from-server" flow;
        Eio.Flow.close flow)
      (fun () ->
        let flow = Eio.Net.connect ~sw env#net addr in
        received := read_all flow);
    !received);;
- : string = "hello-from-server"
```

The same over IPv6 loopback:

```ocaml
# run (fun env ->
    Switch.run @@ fun sw ->
    let addr = `Tcp (Eio.Net.Ipaddr.V6.loopback, 9733) in
    let server = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:5 addr in
    let received = ref "" in
    Fiber.both
      (fun () ->
        let flow, _addr = Eio.Net.accept ~sw server in
        Eio.Flow.copy_string "hello-v6" flow;
        Eio.Flow.close flow)
      (fun () ->
        let flow = Eio.Net.connect ~sw env#net addr in
        received := read_all flow);
    !received);;
- : string = "hello-v6"
```

## A Unix-domain socket

```ocaml
# run (fun env ->
    Switch.run @@ fun sw ->
    let addr = `Unix "eio-mdx-test.sock" in
    let server = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:5 addr in
    let received = ref "" in
    Fiber.both
      (fun () ->
        let flow, _addr = Eio.Net.accept ~sw server in
        Eio.Flow.copy_string "hello-unix" flow;
        Eio.Flow.close flow)
      (fun () ->
        let flow = Eio.Net.connect ~sw env#net addr in
        received := read_all flow);
    !received);;
- : string = "hello-unix"
```

## UDP datagrams

The receiver blocks first (so it is listening before the sender fires):

```ocaml
# run (fun env ->
    Switch.run @@ fun sw ->
    let here = `Udp (Eio.Net.Ipaddr.V4.loopback, 8092) in
    let there = `Udp (Eio.Net.Ipaddr.V4.loopback, 8091) in
    let recv_sock = Eio.Net.datagram_socket ~sw env#net here in
    let got = ref "" in
    Fiber.both
      (fun () ->
        let buf = Cstruct.create 20 in
        let _addr, n = Eio.Net.recv recv_sock buf in
        got := Cstruct.to_string (Cstruct.sub buf 0 n))
      (fun () ->
        let send_sock = Eio.Net.datagram_socket ~sw env#net there in
        Eio.Net.send send_sock ~dst:here [Cstruct.of_string "udp-msg"]);
    !got);;
- : string = "udp-msg"
```

A send is vectored: the buffers are gathered into a single datagram, which the
receiver reads back as their concatenation:

```ocaml
# run (fun env ->
    Switch.run @@ fun sw ->
    let here = `Udp (Eio.Net.Ipaddr.V4.loopback, 8094) in
    let there = `Udp (Eio.Net.Ipaddr.V4.loopback, 8093) in
    let recv_sock = Eio.Net.datagram_socket ~sw env#net here in
    let got = ref "" in
    Fiber.both
      (fun () ->
        let buf = Cstruct.create 20 in
        let _addr, n = Eio.Net.recv recv_sock buf in
        got := Cstruct.to_string (Cstruct.sub buf 0 n))
      (fun () ->
        let send_sock = Eio.Net.datagram_socket ~sw env#net there in
        Eio.Net.send send_sock ~dst:here [Cstruct.of_string "multi-"; Cstruct.of_string "part"]);
    !got);;
- : string = "multi-part"
```

## Listening, connected and accepted sockets all expose a file descriptor

```ocaml
# run (fun env ->
    Switch.run @@ fun sw ->
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9732) in
    let server = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:5 addr in
    let listening = Eio_unix.Resource.fd_opt server <> None in
    let client, accepted =
      Fiber.pair
        (fun () ->
          Eio_unix.Resource.fd_opt (Eio.Net.connect ~sw env#net addr) <> None)
        (fun () ->
          let flow, _addr = Eio.Net.accept ~sw server in
          Eio_unix.Resource.fd_opt flow <> None)
    in
    (listening, client, accepted));;
- : bool * bool * bool = (true, true, true)
```

## Shutting down the receive side

A receive-side `shutdown` cancels only the in-flight reads on that socket, so a
blocked reader wakes up at end-of-file while a concurrent writer on the same
socket is left running (as on POSIX). Here one fiber streams a megabyte through
`a` while another shuts down `a`'s receive side; the writer must still deliver
every byte:

```ocaml
# run (fun _env ->
    Switch.run @@ fun sw ->
    let a, b = Eio_unix.Net.socketpair_stream ~sw () in
    let n = 1_000_000 in
    let msg = String.make n 'x' in
    let received = ref 0 in
    Fiber.all [
      (fun () -> Eio.Flow.copy_string msg a);
      (fun () -> Eio.Flow.shutdown a `Receive);
      (fun () ->
        let buf = Cstruct.create 65536 in
        while !received < n do
          received := !received + Eio.Flow.single_read b buf
        done);
    ];
    !received);;
- : int = 1000000
```

A reader already blocked in `recv` when the receive side is shut down wakes up
and sees end-of-file:

```ocaml
# run (fun _env ->
    Switch.run @@ fun sw ->
    let a, _b = Eio_unix.Net.socketpair_stream ~sw () in
    let got = ref "unset" in
    Fiber.both
      (fun () -> got := read_all a)
      (fun () -> Eio.Flow.shutdown a `Receive);
    !got);;
- : string = ""
```

## Socket options

`setsockopt` and `getsockopt` round-trip on a connected TCP socket. We enable
`TCP_NODELAY` and `SO_KEEPALIVE` on the client end and read them back (the
booleans are rendered with an explicit helper because Windows returns the raw
option value rather than a normalised `0`/`1`):

```ocaml
# run (fun env ->
    Switch.run @@ fun sw ->
    let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9735) in
    let server = Eio.Net.listen env#net ~sw ~reuse_addr:true ~backlog:5 addr in
    let onoff b = if b then "on" else "off" in
    let _, opts =
      Fiber.pair
        (fun () ->
          let flow, _addr = Eio.Net.accept ~sw server in
          Eio.Flow.close flow)
        (fun () ->
          let flow = Eio.Net.connect ~sw env#net addr in
          Eio.Net.setsockopt flow Eio.Net.Sockopt.TCP_NODELAY true;
          Eio.Net.setsockopt flow Eio.Net.Sockopt.SO_KEEPALIVE true;
          Printf.sprintf "TCP_NODELAY=%s SO_KEEPALIVE=%s"
            (onoff (Eio.Net.getsockopt flow Eio.Net.Sockopt.TCP_NODELAY))
            (onoff (Eio.Net.getsockopt flow Eio.Net.Sockopt.SO_KEEPALIVE)))
    in
    opts);;
- : string = "TCP_NODELAY=on SO_KEEPALIVE=on"
```

## Resolving addresses with getaddrinfo

A numeric lookup of the loopback address, filtered to stream and to datagram
protocols. The Windows backend queries each socket type natively and classifies
the results itself (Windows doesn't set `ai_protocol`):

```ocaml
# run (fun env -> Eio.Net.getaddrinfo_stream env#net "127.0.0.1" ~service:"80");;
- : Eio.Net.Sockaddr.stream list = [`Tcp (127.0.0.1, 80)]
```

```ocaml
# run (fun env -> Eio.Net.getaddrinfo_datagram env#net "127.0.0.1" ~service:"80");;
- : Eio.Net.Sockaddr.datagram list = [`Udp (127.0.0.1, 80)]
```

An unfiltered lookup returns both the TCP and UDP entries:

```ocaml
# run (fun env -> Eio.Net.getaddrinfo env#net "127.0.0.1" ~service:"80");;
- : Eio.Net.Sockaddr.t list = [`Tcp (127.0.0.1, 80); `Udp (127.0.0.1, 80)]
```

A lookup that fails raises `Address_lookup_failed` with a structured reason
rather than returning an empty list. The `.invalid` TLD is reserved so it fails
fast without a real DNS round-trip (the exact reason can vary by resolver, so we
only assert the error category here):

```ocaml
# run (fun env ->
    try ignore (Eio.Net.getaddrinfo env#net "nosuchhost.invalid." : _ list)
    with Eio.Io (Eio.Net.E Address_lookup_failed _, _) -> failwith "Address_lookup_failed");;
Exception: Failure "Address_lookup_failed".
```

## Wrapping an existing OS socket

`import_socket_stream` adopts a raw OS file descriptor (here one end of a
`socketpair`) as an Eio flow:

```ocaml
# run (fun _env ->
    Switch.run @@ fun sw ->
    let r, w = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let source = (Eio_unix.Net.import_socket_stream ~sw ~close_unix:true r :> Eio.Flow.source_ty r) in
    let sink = (Eio_unix.Net.import_socket_stream ~sw ~close_unix:true w :> Eio.Flow.sink_ty r) in
    let got = ref "" in
    Fiber.both
      (fun () -> Eio.Flow.copy_string "imported\n" sink)
      (fun () -> got := Eio.Buf_read.(line (of_flow source ~max_size:100)));
    !got);;
- : string = "imported"
```
