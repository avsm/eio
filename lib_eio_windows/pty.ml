open Eio.Std

module Fd = Eio_unix.Fd

type winsize = {
  rows : int;
  cols : int;
  xpixel : int;
  ypixel : int;
}

(* An [HPCON] (pseudoconsole heap object) wrapped in a C custom block. Not a
   kernel handle, so it cannot travel through an [Fd.t]. *)
type conpty

external conpty_create : winsize -> Unix.file_descr -> Unix.file_descr -> conpty = "caml_eio_windows_conpty_create"
external conpty_resize : conpty -> winsize -> unit = "caml_eio_windows_conpty_resize"
external conpty_close : conpty -> unit = "caml_eio_windows_conpty_close"

(* [(in_read, in_write, out_read, out_write)] ends of the two pipe pairs backing
   a pty. Child input: parent -> [in_write] -> [in_read] -> ConPTY. Child output:
   ConPTY -> [out_write] -> [out_read] -> parent. *)
external open_pty_pipes : unit -> Unix.file_descr * Unix.file_descr * Unix.file_descr * Unix.file_descr = "caml_eio_windows_open_pty_pipes"

(* The design proposed a single duplex named pipe used as both hInput and
   hOutput, but ConPTY only ever emitted its init sequence through such a shared
   handle and never rendered child output; the fallback the design anticipated —
   two separate pipe pairs — is used instead. Consequently the "master" is the
   output read end and the "sink" a distinct input write end, rather than one
   bidirectional fd. *)
type t = {
  hpcon : conpty;              (* custom block wrapping the HPCON *)
  master : Fd.t;               (* out_read: the child's (VT-encoded) output *)
  sink_fd : Fd.t;              (* in_write: the child's input *)
  tty : Fd.t;                  (* out_write copy — the "tty" identity token *)
  name : string;               (* synthetic identifier (the pipes are anonymous) *)
  mutable size : winsize;      (* last size given to Create/ResizePseudoConsole *)
}

(* Registry mapping a tty token to its pty, so [Process.spawn_unix ~login_tty]
   can recover the pseudoconsole to attach. Keyed by the tty [Fd.t]'s physical
   identity: [tty t] returns the very [Fd.t] the caller passes back as
   [~login_tty], so pointer equality is exact. Entries are removed on close, so
   the registry does not leak. Guarded by a mutex as [open_pty]/spawn may run in
   different domains. *)
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
  let name =
    Printf.sprintf "conpty-%d-%d" (Unix.getpid ()) (Atomic.fetch_and_add counter 1)
  in
  let in_read, in_write, out_read, out_write = open_pty_pipes () in
  (* If CreatePseudoConsole fails the raw pipe handles are not yet owned by any
     [Fd.t], so close them all here to avoid a leak. *)
  let hpcon =
    try conpty_create size in_read out_write
    with e ->
      List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
        [in_read; in_write; out_read; out_write];
      raise e
  in
  (* ConPTY duplicated [in_read] and [out_write]. Close our [in_read] copy so it
     is not a second reader competing with the console for the child's input;
     [out_write] is kept as the tty token — we never write to it, so it does not
     corrupt the child's output stream. *)
  (try Unix.close in_read with Unix.Unix_error _ -> ());
  (* All ends are synchronous handles: tag them [blocking:true] so
     {!Low_level.classify} routes their I/O to the systhread pool rather than the
     IOCP socket path. [master] is created first so, releases being LIFO, it is
     closed last (letting buffered output drain — see the .mli). *)
  let master = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true out_read in
  let sink_fd = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true in_write in
  let tty = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true out_write in
  let t = { hpcon; master; sink_fd; tty; name; size } in
  register t;
  (* Close order matters (see the .mli): the HPCON tears the console down first,
     then the other ends, and the master last. This release is registered after
     the three [Fd.of_unix ~close_unix:true] releases, so LIFO runs it first. *)
  Switch.on_release sw (fun () ->
      unregister t;
      conpty_close hpcon);
  t

let pty t = t.master
let tty t = t.tty
let name t = t.name
let hpcon t = t.hpcon

let source t = (Flow.of_pipe_source t.master :> Eio_unix.source_ty r)
let sink t = (Flow.of_pipe_source t.sink_fd :> Eio_unix.sink_ty r)

let resize t size =
  conpty_resize t.hpcon size;
  t.size <- size

let get_window_size t = t.size
