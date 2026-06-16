module Pty = Eio_windows.Pty

type t = Pty.t

let open_pty ~sw = Pty.open_pty ~sw ()
let login_tty t = Pty.tty t
let source t = Pty.source t

let resize t ~rows ~cols =
  let d = Pty.get_window_size t in
  Pty.resize t { d with Pty.rows; cols }

let get_size t =
  let d = Pty.get_window_size t in
  (d.Pty.rows, d.Pty.cols)

(* The portable [spawn_unix]'s PATH resolution is POSIX-only, so give the
   command interpreter's absolute path directly. *)
let child_command =
  let comspec =
    try Sys.getenv "COMSPEC" with Not_found -> {|C:\Windows\System32\cmd.exe|}
  in
  (comspec, [ "cmd.exe"; "/c"; "echo"; "PTY_MARKER_73" ])
