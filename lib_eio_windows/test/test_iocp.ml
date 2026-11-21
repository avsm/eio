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

let tests env = List.map (fun (name, fn) -> name, `Quick, fn) [
    "resolver service and error codes", resolve env;
  ]
