(* Pseudoterminal support via the Windows Pseudo Console (ConPTY) API. *)

open Eio.Std

module Fd = Eio_unix.Fd

type winsize = Eio.Pty.winsize

(* A pseudoconsole heap object wrapped in a C custom block. *)
type conpty

external conpty_create : winsize -> Unix.file_descr -> Unix.file_descr -> conpty = "caml_eio_windows_conpty_create"
external conpty_resize : conpty -> winsize -> unit = "caml_eio_windows_conpty_resize"
external conpty_close : conpty -> unit = "caml_eio_windows_conpty_close"
external open_pty_pipes : unit -> Unix.file_descr * Unix.file_descr * Unix.file_descr * Unix.file_descr = "caml_eio_windows_open_pty_pipes"

type t = {
  hpcon : conpty;              (* custom block wrapping the HPCON *)
  source : Fd.t;               (* out_read: the child's output *)
  sink : Fd.t;                 (* in_write: the child's input *)
  name : string;               (* synthetic identifier as pipes are anonymous *)
  mutable size : winsize;      (* last size given to Create/ResizePseudoConsole *)
}

module Impl = struct
  type nonrec t = t

  let name t = t.name

  let resize t size =
    conpty_resize t.hpcon size;
    t.size <- size

  let window_size t = t.size

  let read_methods = []
  let single_read t buf = Flow.Pipe_impl.single_read t.source buf
  let single_write t bufs = Flow.Pipe_impl.single_write t.sink bufs
  let copy t ~src = Flow.Pipe_impl.copy t.sink ~src

  let send_keys t s = ignore (Flow.Pipe_impl.single_write t.sink [ Cstruct.of_string s ] : int)
  let interrupt t = send_keys t "\x03"
  let send_eof t = send_keys t "\x1a\r"

  let tty _ = None
  let pty _ = None
end

type (_, _, _) Eio.Resource.pi +=
  | Conpty : ('t, 't -> conpty, [> Eio.Pty.pty_ty]) Eio.Resource.pi

let handler =
  Eio.Resource.handler [
    H (Conpty, (fun t -> t.hpcon));
    H (Eio_unix.Pty.Pi.Unix_pty, (module Impl));
    H (Eio.Pty.Pi.Pty, (module Impl));
    H (Eio.Flow.Pi.Source, (module Impl));
    H (Eio.Flow.Pi.Sink, (module Impl));
  ]

let conpty (Eio.Resource.T (v, ops) : _ Eio.Pty.t) =
  match Eio.Resource.get_opt ops Conpty with
  | Some f -> f v
  | None -> Fmt.invalid_arg "spawn: ~tty is not an eio_windows pty"

(* Unique per-process pty-name counter. *)
let counter = Atomic.make 0

let open_pty ~sw ?(size = Eio.Pty.default_winsize) () =
  try
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
    (* The pseudoconsole holds its own references to these ends, so close ours;
       the source then reaches end-of-file once the pseudoconsole is closed. *)
    List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ())
      [in_read; out_write];
    let source = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true out_read in
    let sink = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true in_write in
    let t = { hpcon; source; sink; name; size } in
    Switch.on_release sw (fun () -> conpty_close hpcon);
    (Eio.Resource.T (t, handler) : Eio.Pty.ty r)
  with
  | Unix.Unix_error (Unix.EOPNOTSUPP, _, _) -> raise (Eio.Pty.err Eio.Pty.Unsupported)
  | Unix.Unix_error (code, name, arg) ->
    raise (Eio.Pty.err (Eio.Pty.Open_failed (Eio_unix.Unix_error (code, name, arg))))
