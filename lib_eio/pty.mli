(** A pseudoterminal is a pair of connected endpoints emulating a terminal.
    The {e pseudoterminal device} is used by a controlling program such as a
    terminal emulator, while the {e terminal device} is used by a child
    process as its controlling terminal.

    This module is the portable view of the controlling side. A {!t} is a
    bidirectional {!Flow} where reading gives the child's output and writing
    gives the child input.

    Once the terminal side has been closed, reading the pty raises
    [End_of_file] and writing it raises a {!Net.E} [(Connection_reset _)]. *)

open Std

(** {2 Errors} *)

type error =
  | Unsupported
  (** The platform has no pseudoterminal support at all.
      Callers may want to fall back to spawning with plain pipes. *)
  | Open_failed of Exn.Backend.t
  (** The OS could not allocate a pseudoterminal. *)

type Exn.err += E of error

val err : error -> exn
(** [err e] is [Eio.Exn.create (E e)] *)

(** {2 Types} *)

(** Terminal window dimensions. *)
type winsize = {
  rows : int;     (** Height of the terminal in character rows. *)
  cols : int;     (** Width of the terminal in character columns. *)
  xpixel : int;   (** Width in pixels ([0] if unknown). *)
  ypixel : int;   (** Height in pixels ([0] if unknown). *)
}

val default_winsize : winsize
(** 24 rows by 80 columns, with unknown pixel sizes. *)

type pty_ty = [`Pty]
type ty = [pty_ty | Flow.source_ty | Flow.sink_ty]

type 'a t = ([> ty] as 'a) r
(** A pseudoterminal, as seen from the controlling side. *)

val name : _ t -> string
(** [name t] identifies the terminal (e.g. the path of the terminal device,
    on systems where it has one). *)

val resize : _ t -> winsize -> unit
(** [resize t size] sets the terminal window size. *)

val window_size : _ t -> winsize
(** [window_size t] is the current terminal window size. *)

val interrupt : _ t -> unit
(** [interrupt t] does what the user's interrupt keystroke (Ctrl-C) would do:
    requests the terminal to interrupt the child attached to it. *)

val send_eof : _ t -> unit
(** [send_eof t] signals end-of-input to the child as a keystroke would.
    Like {!interrupt}, this is interpreted by the terminal's input cooking,
    so a raw-mode child sees the control characters as input instead. *)

(** {2 Provider Interface} *)

module Pi : sig
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

  val pty : (module FLOW_PTY with type t = 't) -> ('t, ty) Resource.handler
end
