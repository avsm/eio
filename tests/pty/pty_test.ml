(* Tests for the portable pseudoterminal API.

   The transcript is identical on POSIX and Windows, but the child commands
   differ per platform. *)

(* Force '\n' newlines so \r\n does not break the mdx comparison on Windows. *)
let () = set_binary_mode_out stdout true

open Eio.Std

module Pty = Eio.Pty

let say fmt = Printf.ksprintf (fun s -> print_string s; print_char '\n'; flush stdout) fmt

let failures = ref 0
let check msg ok = if not ok then (incr failures; say "FAIL: %s" msg)

let marker = "PTY_MARKER_73"

(* [spawn]'s PATH resolution is POSIX-only, so executables are given absolutely. *)
let comspec =
  try Sys.getenv "COMSPEC" with Not_found -> {|C:\Windows\System32\cmd.exe|}

let print_marker_command =
  if Sys.win32 then (comspec, [ "cmd.exe"; "/c"; "echo"; marker ])
  else ("/bin/sh", [ "sh"; "-c"; "echo " ^ marker ])

(* Reads one line and replies with it. The POSIX child then holds the terminal
   open, as some BSDs discard pending output once the last tty fd closes. *)
let reply_command =
  if Sys.win32 then
    (comspec, [ "cmd.exe"; "/v:on"; "/c"; "set /p line=& echo reply: !line!" ])
  else
    ("/bin/sh", [ "sh"; "-c"; {|read line; echo "reply: $line"; read keep_open|} ])

(* A console delivers input on CR; a pty line discipline on NL. *)
let input_line_text = if Sys.win32 then "hello\r" else "hello\n"

(* Prints READY once running, then outlives the test timeout. Interrupt keys
   are discarded until the child is the foreground process group. *)
let wait_command =
  if Sys.win32 then (comspec, [ "cmd.exe"; "/c"; "echo READY& ping -n 30 127.0.0.1 >nul" ])
  else ("/bin/sh", [ "sh"; "-c"; "echo READY; exec sleep 30" ])

(* Copies terminal input to output until end-of-file. *)
let cat_command =
  if Sys.win32 then (comspec, [ "cmd.exe"; "/c"; "more" ])
  else ("/bin/cat", [ "cat" ])

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  go 0

(* Scans chunks for [needle] until it appears or the pty hangs up.
   Callers bound this with a timeout. *)
let read_until ~needle src =
  let buf = Cstruct.create 4096 in
  let acc = Buffer.create 256 in
  let rec loop () =
    if contains ~needle (Buffer.contents acc) then true
    else
      match Eio.Flow.single_read src buf with
      | got ->
        Buffer.add_string acc (Cstruct.to_string (Cstruct.sub buf 0 got));
        loop ()
      | exception End_of_file -> contains ~needle (Buffer.contents acc)
  in
  loop ()

let with_timeout env fn =
  Eio.Time.with_timeout env#clock 10. (fun () -> Ok (fn ())) = Ok true

(* A child's exit can block until pending output such as key echo is consumed,
   so drain the pty whenever nothing else reads it. *)
let drain_in_background ~sw t =
  Fiber.fork_daemon ~sw (fun () ->
      ignore (Eio.Flow.read_all t : string);
      `Stop_daemon)

let spawn_on_tty ~sw env t (executable, args) =
  Eio.Process.spawn ~sw env#process_mgr ~tty:t ~executable args

(* Runs [fn] with a fresh pty under its own switch. *)
let with_pty env fn =
  Switch.run @@ fun sw ->
  fn sw (Eio.Process.open_pty ~sw env#process_mgr ())

(* Scans the pty for [needle], bounded by the timeout. *)
let scan env t needle = with_timeout env (fun () -> read_until ~needle t)

let winsize rows cols = { Pty.rows; cols; xpixel = 0; ypixel = 0 }

let test_open_and_resize env =
  with_pty env @@ fun _sw t ->
  say "open: ok";
  let { Pty.rows; cols; _ } = Pty.window_size t in
  say "size: %dx%d" rows cols;
  Pty.resize t (winsize 40 100);
  let { Pty.rows; cols; _ } = Pty.window_size t in
  say "resize: %dx%d" rows cols;
  let name = Pty.name t in
  let prefix = if Sys.win32 then "conpty-" else "/dev/" in
  if String.starts_with ~prefix name then say "name: ok"
  else say "name: unexpected %S" name;
  if not Sys.win32 then begin
    (* Either end's fd sees the same size. *)
    let ws = Eio_unix.Pty.get_window_size (Eio_unix.Pty.tty t) in
    check "size visible on the tty end" (ws.Pty.rows = 40 && ws.Pty.cols = 100);
    Eio_unix.Pty.set_window_size (Eio_unix.Pty.pty t) (winsize 50 90);
    check "size set via the pty fd" ((Pty.window_size t).Pty.rows = 50)
  end

let test_child_output env =
  with_pty env @@ fun sw t ->
  let child = spawn_on_tty ~sw env t print_marker_command in
  say "spawn: ok";
  let found = scan env t marker in
  say "marker: %s" (if found then "found" else "TIMEOUT");
  let code =
    match Eio.Process.await child with
    | `Exited n -> n
    | `Signaled n -> 128 + n
  in
  say "exit: %d" code

(* The child is not awaited; the POSIX one exits when the switch closes the tty. *)
let test_child_input env =
  with_pty env @@ fun sw t ->
  let _child = spawn_on_tty ~sw env t reply_command in
  Eio.Flow.copy_string input_line_text t;
  let found = scan env t "reply: hello" in
  say "input: %s" (if found then "reply found" else "TIMEOUT")

(* Only exit is checked: the exit status differs per platform. *)
let test_interrupt env =
  with_pty env @@ fun sw t ->
  let child = spawn_on_tty ~sw env t wait_command in
  let ready = scan env t "READY" in
  Pty.interrupt t;
  drain_in_background ~sw t;
  let exited =
    ready
    && with_timeout env (fun () ->
        ignore (Eio.Process.await child : Eio.Process.exit_status);
        true)
  in
  say "interrupt: %s" (if exited then "ok" else "TIMEOUT")

let check_echo env =
  with_pty env @@ fun sw t ->
  let _child = spawn_on_tty ~sw env t cat_command in
  Eio.Flow.copy_string input_line_text t;
  check "input is echoed" (scan env t "hello")

let check_send_eof env =
  with_pty env @@ fun sw t ->
  let child = spawn_on_tty ~sw env t cat_command in
  drain_in_background ~sw t;
  Pty.send_eof t;
  check "send_eof ends cat"
    (with_timeout env (fun () -> Eio.Process.await child = `Exited 0))

(* With echo on, the input comes back before the reply. *)
let check_interact_echo env =
  with_pty env @@ fun sw t ->
  let _child = spawn_on_tty ~sw env t reply_command in
  Eio.Flow.copy_string input_line_text t;
  let r = Eio.Buf_read.of_flow t ~max_size:1024 in
  check "input echoed before the reply"
    (with_timeout env (fun () ->
         Eio.Buf_read.lines r |> Seq.take 2 |> List.of_seq
         = [ "hello"; "reply: hello" ]))

(* With echo off, the reply is the first line to reach the pty. *)
let check_interact_no_echo env =
  with_pty env @@ fun sw t ->
  let tty = Eio_unix.Pty.tty t in
  let attr = Eio_unix.Pty.Tc.getattr tty in
  Eio_unix.Pty.Tc.setattr tty Unix.TCSANOW { attr with c_echo = false };
  let _child = spawn_on_tty ~sw env t reply_command in
  Eio.Flow.copy_string input_line_text t;
  let r = Eio.Buf_read.of_flow t ~max_size:1024 in
  check "no echo before the reply"
    (with_timeout env (fun () -> Eio.Buf_read.line r = "reply: hello"))

(* Exercises the fiber-blocking paths: setattr via drain, then flush and flow. *)
let check_termios env =
  with_pty env @@ fun _sw t ->
  let module Tc = Eio_unix.Pty.Tc in
  let tty = Eio_unix.Pty.tty t in
  let attr = Tc.getattr tty in
  Tc.setattr tty Unix.TCSADRAIN { attr with c_echo = false };
  Tc.drain tty;
  Tc.flush tty Unix.TCIOFLUSH;
  Tc.flow tty Unix.TCOON;
  check "echo attribute cleared" (not (Tc.getattr tty).c_echo)

(* [~tty] makes the terminal the child's controlling terminal and standard
   streams; without it the child has no terminal at all. *)
let check_controlling_terminal env =
  (with_pty env @@ fun sw t ->
   let child =
     spawn_on_tty ~sw env t
       ("/bin/sh", [ "sh"; "-c"; "[ -t 0 ] && [ -t 1 ] && [ -t 2 ]" ])
   in
   check "tty gives a controlling terminal" (Eio.Process.await child = `Exited 0));
  Switch.run (fun sw ->
      (* Buffer-backed stdio gives the child pipes, not a terminal. *)
      let out = Buffer.create 16 in
      let child =
        Eio.Process.spawn ~sw env#process_mgr
          ~stdin:(Eio.Flow.string_source "")
          ~stdout:(Eio.Flow.buffer_sink out)
          ~stderr:(Eio.Flow.buffer_sink out)
          ~executable:"/bin/sh" [ "sh"; "-c"; "[ -t 0 ]" ]
      in
      check "no controlling terminal without a tty" (Eio.Process.await child = `Exited 1))

(* [~login_tty] plus an explicit standard stream is rejected.
   Windows has no [~login_tty] at all. *)
let check_fd_conflict env =
  with_pty env @@ fun sw t ->
  let executable, args = print_marker_command in
  match
    Eio_unix.Process.spawn_unix ~sw env#process_mgr ~login_tty:(Eio_unix.Pty.tty t)
      ~fds:[ 0, Eio_unix.Fd.stdin, `Blocking ] ~executable args
  with
  | _child -> check "login_tty + fds conflict rejected" false
  | exception Invalid_argument _ -> ()

let () =
  Eio_main.run @@ fun env ->
  test_open_and_resize env;
  test_child_output env;
  test_child_input env;
  test_interrupt env;
  check_echo env;
  check_send_eof env;
  (* POSIX only: these need a shell, or exercise the [Eio_unix] extras. *)
  if not Sys.win32 then begin
    check_interact_echo env;
    check_interact_no_echo env;
    check_termios env;
    check_controlling_terminal env;
    check_fd_conflict env
  end;
  say "checks: %s" (if !failures = 0 then "ok" else "FAILED");
  flush stdout;
  if !failures > 0 then exit 1
