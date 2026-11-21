open Eio.Std

let late_registration ~close_unix ~remove ~fail =
  let module Fd = Eio_unix.Fd in
  Switch.run @@ fun sw ->
  let raw = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
  let fd = Fd.of_unix ~sw ~close_unix raw in
  let released = ref 0 in
  let go, start = Promise.create () in
  let borrower = Fiber.fork_promise ~sw (fun () ->
      Fd.use_exn "test" fd (fun _ ->
          Promise.await go;
          Eio_unix.Private.Windows_fd.set_detach fd (fun () ->
              ignore (Unix.getsockopt raw SO_REUSEADDR);
              incr released;
              if fail then raise Exit))) in
  let closer = Fiber.fork_promise ~sw (fun () ->
      if remove then assert (Fd.remove fd = Some raw) else Fd.close fd) in
  assert (not (Fd.is_open fd));
  Promise.resolve start ();
  let owned_close = close_unix && not remove in
  let check_result p ~raises =
    match Promise.await_exn p with
    | () -> assert (not raises)
    | exception Exit -> assert raises
  in
  check_result borrower ~raises:(fail && owned_close);
  check_result closer ~raises:(fail && not owned_close);
  Alcotest.(check int) "detach count" 1 !released;
  match Unix.getsockopt raw SO_REUSEADDR with
  | _ -> Unix.close raw; assert (not close_unix || (remove && not fail))
  | exception Unix.Unix_error ((EBADF | ENOTSOCK), _, _) ->
    assert (close_unix && (not remove || fail))

let tests = [
  "registration after close starts", `Quick, (fun () ->
      List.iter (fun close_unix ->
          List.iter (fun remove ->
              List.iter (fun fail ->
                  late_registration ~close_unix ~remove ~fail) [false; true]) [false; true]) [false; true])
]
