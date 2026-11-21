open Eio.Std

let resolve env () =
  let host = "127.0.0.1" in
  let lo = Eio.Net.Ipaddr.V4.loopback in
  let got = Eio.Net.getaddrinfo ~service:"80" env#net host in
  List.iter (fun addr -> Alcotest.(check bool) "numeric service" true (List.mem addr got))
    [`Tcp (lo, 80); `Udp (lo, 80)];
  Alcotest.(check bool) "TCP-only service" true
    (Eio.Net.getaddrinfo ~service:"http" env#net host = [`Tcp (lo, 80)]);
  Alcotest.(check bool) "IPv6 literal" true
    (List.mem (`Tcp (Eio.Net.Ipaddr.V6.loopback, 0)) (Eio.Net.getaddrinfo env#net "::1"));
  match Eio.Net.getaddrinfo ~service:"nosuchservice" env#net host with
  | _ -> Alcotest.fail "resolved an unknown service"
  | exception Eio.Io (Eio.Net.E (Address_lookup_failed SERVICE), _) -> ()

let close_during_readiness env () =
  Switch.run @@ fun sw ->
  let r, w = Eio_unix.Net.socketpair_stream ~sw () in
  Eio.Time.with_timeout_exn env#clock 5.0 (fun () ->
      let ready = Fiber.fork_promise ~sw (fun () ->
          Eio_windows.Low_level.await_readable (Eio_unix.Resource.fd r)) in
      Eio.Flow.close r;
      Eio_windows.Low_level.await_writable (Eio_unix.Resource.fd w);
      Eio.Flow.copy_string "x" w;
      Promise.await_exn ready)

let tests () = List.map (fun (name, fn) -> name, `Quick, fn) [
    "close during low-level readiness wait", close_during_readiness;
    "backend identifier", (fun env () -> Alcotest.(check string) "backend" "windows" env#backend_id);
    "resolver service and error codes", resolve;
  ]
