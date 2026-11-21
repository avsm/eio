let invalid_address net (node, service) =
  match Eio.Net.getaddrinfo net ~service node with
  | _ -> failwith "resolved a truncated name"
  | exception Eio.Io (Eio.Net.E (Address_lookup_failed NONAME), _) -> ()

let negative_offsets dir =
  Eio.Switch.run @@ fun sw ->
  let path = Eio.Path.(dir / "positioned-offset") in
  let file = Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) path in
  Fun.protect ~finally:(fun () -> Eio.Flow.close file; Eio.Path.unlink path) @@ fun () ->
  Eio.Flow.copy_string "1234" file;
  let file_offset = Optint.Int63.minus_one in
  let bufs = List.map Cstruct.of_string ["a"; "b"] in
  let rejected fn =
    match fn () with
    | () -> failwith "accepted negative offset"
    | exception Invalid_argument _ -> ()
  in
  rejected (fun () -> Eio.File.pwrite_all file ~file_offset bufs);
  rejected (fun () -> ignore (Eio.File.pwrite_single file ~file_offset bufs));
  rejected (fun () -> ignore (Eio.File.pread file ~file_offset bufs));
  rejected (fun () -> Eio.File.pread_exact file ~file_offset bufs);
  assert (Eio.Path.load path = "1234")

let () =
  Eio_main.run @@ fun env ->
  negative_offsets env#cwd;
  List.iter (invalid_address env#net) ["127.0.0.1\000invalid", "80"; "127.0.0.1", "80\000invalid"]
