let invalid_address net (node, service) =
  match Eio.Net.getaddrinfo net ~service node with
  | _ -> failwith "resolved a truncated name"
  | exception Eio.Io (Eio.Net.E (Address_lookup_failed NONAME), _) -> ()

let () =
  Eio_main.run @@ fun env ->
  List.iter (invalid_address env#net) ["127.0.0.1\000invalid", "80"; "127.0.0.1", "80\000invalid"]
