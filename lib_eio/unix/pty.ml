open Eio.Std

type winsize = Eio.Pty.winsize = {
  rows : int;
  cols : int;
  xpixel : int;
  ypixel : int;
}

external create : unit -> Unix.file_descr = "eio_unix_open_pty"

(* Not domain-safe on platforms without [ptsname_r] *)
external get_pty_peer : Unix.file_descr -> Unix.file_descr * string = "eio_unix_get_pty_peer"

external get_winsize : Unix.file_descr -> winsize = "eio_unix_get_winsize"
external set_winsize : Unix.file_descr -> winsize -> unit = "eio_unix_set_winsize"

let get_window_size fd = Fd.use_exn "get_window_size" fd get_winsize
let set_window_size fd ws = Fd.use_exn "set_window_size" fd (fun fd -> set_winsize fd ws)

module Pi = struct
  module type UNIX_PTY = sig
    include Eio.Pty.Pi.FLOW_PTY

    val tty : t -> Fd.t option
    val pty : t -> Fd.t option
  end

  type (_, _, _) Eio.Resource.pi +=
    | Unix_pty : ('t, (module UNIX_PTY with type t = 't), [> Eio.Pty.pty_ty]) Eio.Resource.pi

  let unix_pty (type t) (module X : UNIX_PTY with type t = t) =
    Eio.Resource.handler [
      H (Unix_pty, (module X));
      H (Eio.Pty.Pi.Pty, (module X));
      H (Eio.Flow.Pi.Source, (module X));
      H (Eio.Flow.Pi.Sink, (module X));
    ]
end

let tty (t : [> Eio.Pty.pty_ty] r) =
  let (Eio.Resource.T (v, ops)) = t in
  let module X = (val (Eio.Resource.get ops Pi.Unix_pty)) in
  match X.tty v with
  | Some fd -> fd
  | None -> raise (Unix.Unix_error (Unix.EOPNOTSUPP, "tty", ""))

let pty (t : [> Eio.Pty.pty_ty] r) =
  let (Eio.Resource.T (v, ops)) = t in
  let module X = (val (Eio.Resource.get ops Pi.Unix_pty)) in
  match X.pty v with
  | Some fd -> fd
  | None -> raise (Unix.Unix_error (Unix.EOPNOTSUPP, "pty", ""))

(* A pty raises [EIO] once the terminal side has been closed (a hang-up).
   Normalise it so hang-ups are portable: end-of-stream on read and
   [Connection_reset] on write, as the Windows backend reports. *)
let hangup_is_eof fn =
  try fn () with
  | Unix.Unix_error (Unix.EIO, _, _)
  | Eio.Io (Eio.Exn.X (Types.Unix_error (Unix.EIO, _, _)), _) ->
    raise End_of_file

let hangup_is_reset fn =
  try fn () with
  | Unix.Unix_error (Unix.EIO, name, arg) ->
    raise (Eio.Net.err (Eio.Net.Connection_reset (Types.Unix_error (Unix.EIO, name, arg))))
  | Eio.Io (Eio.Exn.X (Types.Unix_error (Unix.EIO, _, _) as e), _) ->
    raise (Eio.Net.err (Eio.Net.Connection_reset e))

module Posix = struct
  type t = {
    flow : [Types.source_ty | Types.sink_ty] r;   (* The pseudoterminal-device end *)
    master : Fd.t;
    tty_fd : Fd.t;
    name : string;
  }

  let name t = t.name
  let resize t ws = set_window_size t.master ws
  let window_size t = get_window_size t.master

  let read_methods = []
  let single_read t buf = hangup_is_eof (fun () -> Eio.Flow.single_read t.flow buf)
  let single_write t bufs = hangup_is_reset (fun () -> Eio.Flow.single_write t.flow bufs)
  let copy t ~src = hangup_is_reset (fun () -> Eio.Flow.copy src t.flow)

  let send_control_char t ~fallback get =
    let c =
      match get (Fd.use_exn "tcgetattr" t.tty_fd Unix.tcgetattr) with
      | '\000' -> fallback
      | c -> c in
    hangup_is_reset (fun () -> Eio.Flow.write t.flow [ Cstruct.of_string (String.make 1 c) ])

  let interrupt t = send_control_char t ~fallback:'\003' (fun a -> a.Unix.c_vintr)
  let send_eof t = send_control_char t ~fallback:'\004' (fun a -> a.Unix.c_veof)

  let tty t = Some t.tty_fd
  let pty t = Some t.master
end

let posix_handler = Pi.unix_pty (module Posix)

let open_posix ~sw ~size =
  try
    let pty_fd = create () in
    Unix.set_nonblock pty_fd;
    let flow : [Types.source_ty | Types.sink_ty] r =
      Net.import_socket_stream ~sw ~close_unix:true pty_fd in
    let master = Resource.fd flow in
    let tty_fd, name = get_pty_peer pty_fd in
    let tty_fd = Fd.of_unix ~sw ~blocking:true ~seekable:false ~close_unix:true tty_fd in
    (* Some systems (e.g. macos) reject winsize ioctls on the pty end until the
       terminal device has been opened, so set the initial size via the tty. *)
    set_window_size tty_fd size;
    Eio.Resource.T ({ Posix.flow; master; tty_fd; name }, posix_handler)
  with
  | Unix.Unix_error (Unix.EOPNOTSUPP, _, _) -> raise (Eio.Pty.err Eio.Pty.Unsupported)
  | Unix.Unix_error (code, name, arg) ->
    raise (Eio.Pty.err (Eio.Pty.Open_failed (Types.Unix_error (code, name, arg))))

module Tc = struct
  let getattr fd = Fd.use_exn "tcgetattr" fd Unix.tcgetattr

  let setattr fd when_ attr =
    Fd.use_exn "tcsetattr" fd (fun fd ->
        match when_ with
        | Unix.TCSANOW -> Unix.tcsetattr fd when_ attr
        | TCSADRAIN | TCSAFLUSH ->
          Thread_pool.run_in_systhread ~label:"tcsetattr"
            (fun () -> Unix.tcsetattr fd when_ attr))

  let sendbreak fd duration =
    Fd.use_exn "tcsendbreak" fd (fun fd ->
        Thread_pool.run_in_systhread ~label:"tcsendbreak"
          (fun () -> Unix.tcsendbreak fd duration))

  let drain fd =
    Fd.use_exn "tcdrain" fd (fun fd ->
        Thread_pool.run_in_systhread ~label:"tcdrain" (fun () -> Unix.tcdrain fd))

  let flush fd queue = Fd.use_exn "tcflush" fd (fun fd -> Unix.tcflush fd queue)
  let flow fd action = Fd.use_exn "tcflow" fd (fun fd -> Unix.tcflow fd action)
end
