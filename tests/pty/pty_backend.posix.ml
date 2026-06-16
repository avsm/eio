open Eio.Std

module Pty = Eio_unix.Pty

type t = Pty.t

let open_pty ~sw = Pty.open_pty ~sw ()
let login_tty t = Pty.tty t
let source t = (Pty.source t :> Eio_unix.source_ty r)

(* Set on the pty (master) end and read it back from the same end. *)
let resize t ~rows ~cols =
  Pty.set_window_size (Pty.pty t) { Pty.rows; cols; xpixel = 0; ypixel = 0 }

let get_size t =
  let d = Pty.get_window_size (Pty.pty t) in
  (d.Pty.rows, d.Pty.cols)

(* Absolute path: the portable [spawn_unix]'s PATH resolution is POSIX-only, and
   an absolute executable avoids depending on it at all. *)
let child_command = ("/bin/sh", [ "sh"; "-c"; "echo PTY_MARKER_73" ])
