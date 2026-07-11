open Eio.Std

module Fd = Eio_unix.Fd

type winsize = {
  rows : int;
  cols : int;
  xpixel : int;
  ypixel : int;
}

(* A pseudoconsole heap object wrapped in a C custom block. *)
type conpty

external conpty_create : winsize -> Unix.file_descr -> Unix.file_descr -> conpty = "caml_eio_windows_conpty_create"
external conpty_resize : conpty -> winsize -> unit = "caml_eio_windows_conpty_resize"
external conpty_close : conpty -> unit = "caml_eio_windows_conpty_close"
external open_pty_pipes : unit -> Unix.file_descr * Unix.file_descr * Unix.file_descr * Unix.file_descr = "caml_eio_windows_open_pty_pipes"

(* Unlike the unix backend, the pty side here are two simplex pipes:
   one to read the child's output and one to write its input. *)
type t = {
  hpcon : conpty;              (* custom block wrapping the HPCON *)
  pty_source : Fd.t;           (* out_read: the child's output *)
  pty_sink : Fd.t;             (* in_write: the child's input *)
  tty : Fd.t;                  (* the "tty" identity token *)
  name : string;               (* synthetic identifier as pipes are anonymous *)
  mutable size : winsize;      (* last size given to Create/ResizePseudoConsole *)
}

(* Map a tty token to its pty *)
let registry_mutex = Mutex.create ()
let registry : (Fd.t * t) list ref = ref []

let register t =
  Mutex.lock registry_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock registry_mutex)
    (fun () -> registry := (t.tty, t) :: !registry)

let unregister t =
  Mutex.lock registry_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock registry_mutex)
    (fun () -> registry := List.filter (fun (fd, _) -> not (fd == t.tty)) !registry)

let lookup fd =
  Mutex.lock registry_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock registry_mutex)
    (fun () -> List.find_map (fun (k, v) -> if k == fd then Some v else None) !registry)

(* Unique per-process pipe-name counter. *)
let counter = Atomic.make 0

let default_size = { rows = 24; cols = 80; xpixel = 0; ypixel = 0 }

let open_pty ~sw ?(size = default_size) () =
  (* TODO avsm: is there a better uuid than Unix.getpid() here? *)
  let name = Printf.sprintf "conpty-%d-%d" (Unix.getpid ()) (Atomic.fetch_and_add counter 1) in
  let in_read, in_write, out_read, out_write = open_pty_pipes () in
  let hpcon =
    try conpty_create size in_read out_write
    with e ->
      List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
        [in_read; in_write; out_read; out_write];
      raise e
  in
  (try Unix.close in_read with Unix.Unix_error _ -> ());
  let pty_source = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true out_read in
  let pty_sink = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true in_write in
  let tty = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true out_write in
  let t = { hpcon; pty_source; pty_sink; tty; name; size } in
  register t;
  Switch.on_release sw (fun () -> unregister t; conpty_close hpcon);
  t

let pty t = t.pty_source
let tty t = t.tty
let name t = t.name
let hpcon t = t.hpcon

let source t = (Flow.of_pipe_source t.pty_source :> Eio_unix.source_ty r)
let sink t = (Flow.of_pipe_source t.pty_sink :> Eio_unix.sink_ty r)

let resize t size =
  conpty_resize t.hpcon size;
  t.size <- size

let get_window_size t = t.size
