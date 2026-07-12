open Std

type error =
  | Unsupported
  | Open_failed of Exn.Backend.t

type Exn.err += E of error

let err e = Exn.create (E e)

let () =
  Exn.register_pp (fun f -> function
      | E e ->
        Fmt.string f "Pty ";
        begin match e with
          | Unsupported -> Fmt.string f "Unsupported"
          | Open_failed e -> Fmt.pf f "Open_failed %a" Exn.Backend.pp e
        end;
        true
      | _ -> false
    )

type winsize = {
  rows : int;
  cols : int;
  xpixel : int;
  ypixel : int;
}

let default_winsize = { rows = 24; cols = 80; xpixel = 0; ypixel = 0 }

type pty_ty = [`Pty]
type ty = [pty_ty | Flow.source_ty | Flow.sink_ty]
type 'a t = ([> ty] as 'a) r

module Pi = struct
  module type PTY = sig
    type t

    val name : t -> string
    val resize : t -> winsize -> unit
    val window_size : t -> winsize
    val interrupt : t -> unit
    val send_eof : t -> unit
  end

  type (_, _, _) Resource.pi +=
    | Pty : ('t, (module PTY with type t = 't), [> pty_ty]) Resource.pi

  module type FLOW_PTY = sig
    include PTY
    include Flow.Pi.SOURCE with type t := t
    include Flow.Pi.SINK with type t := t
  end

  let pty (type t) (module X : FLOW_PTY with type t = t) =
    Resource.handler [
      H (Pty, (module X));
      H (Flow.Pi.Source, (module X));
      H (Flow.Pi.Sink, (module X));
    ]
end

let name (t : [> pty_ty] r) =
  let (Resource.T (v, ops)) = t in
  let module X = (val (Resource.get ops Pi.Pty)) in
  X.name v

let resize (t : [> pty_ty] r) size =
  let (Resource.T (v, ops)) = t in
  let module X = (val (Resource.get ops Pi.Pty)) in
  X.resize v size

let window_size (t : [> pty_ty] r) =
  let (Resource.T (v, ops)) = t in
  let module X = (val (Resource.get ops Pi.Pty)) in
  X.window_size v

let interrupt (t : [> pty_ty] r) =
  let (Resource.T (v, ops)) = t in
  let module X = (val (Resource.get ops Pi.Pty)) in
  X.interrupt v

let send_eof (t : [> pty_ty] r) =
  let (Resource.T (v, ops)) = t in
  let module X = (val (Resource.get ops Pi.Pty)) in
  X.send_eof v
