(** OS-level access to pseudoterminals *)

open Eio.Std

type winsize = Eio.Pty.winsize = {
  rows : int;     (** Height of the terminal in character rows. *)
  cols : int;     (** Width of the terminal in character columns. *)
  xpixel : int;   (** Width in pixels ([0] if unknown). *)
  ypixel : int;   (** Height in pixels ([0] if unknown). *)
}

(** {2 File descriptors} *)

val tty : _ Eio.Pty.t -> Fd.t
(** [tty t] is the terminal-device end, to pass as [~login_tty] when spawning
    a child with {!Process.spawn_unix}. *)

val pty : _ Eio.Pty.t -> Fd.t
(** [pty t] is the pseudoterminal-device (controlling) end. *)

(** {2 Window sizes of arbitrary terminals} *)

val get_window_size : Fd.t -> winsize
(** [get_window_size fd] returns the window size of terminal [fd].

    @raise Unix.Unix_error if [fd] is not a terminal. *)

val set_window_size : Fd.t -> winsize -> unit
(** [set_window_size fd ws] sets the window size of terminal [fd].

    Setting it on the {!pty} end updates the terminal and delivers [SIGWINCH]
    to the foreground process group attached to the terminal.

    @raise Unix.Unix_error if [fd] is not a terminal. *)

(** {2 Terminal attributes}

    Terminal attributes control.
    These raise [Unix.Unix_error] if [fd] is not a terminal. *)

module Tc : sig
  val getattr : Fd.t -> Unix.terminal_io
  (** [getattr fd] returns the current terminal attributes of [fd].
      See {!Unix.tcgetattr}. *)

  val setattr : Fd.t -> Unix.setattr_when -> Unix.terminal_io -> unit
  (** [setattr fd when_ attr] sets the terminal attributes of [fd].

      With [TCSADRAIN] or [TCSAFLUSH] the change waits for pending output to
      drain in the current fiber.
      See {!Unix.tcsetattr}. *)

  val sendbreak : Fd.t -> int -> unit
  (** [sendbreak fd duration] sends a break condition on [fd].

      This blocks the current fiber while the break is transmitted.
      See {!Unix.tcsendbreak}. *)

  val drain : Fd.t -> unit
  (** [drain fd] waits until all output written to [fd] has been transmitted.

      This blocks the current fiber until the output drains.
      See {!Unix.tcdrain}. *)

  val flush : Fd.t -> Unix.flush_queue -> unit
  (** [flush fd queue] discards pending input and/or output on [fd].
      See {!Unix.tcflush}. *)

  val flow : Fd.t -> Unix.flow_action -> unit
  (** [flow fd action] suspends or resumes transmission/reception on [fd].
      See {!Unix.tcflow}. *)
end

(** {2 Provider Interface} *)

module Pi : sig
  module type UNIX_PTY = sig
    include Eio.Pty.Pi.FLOW_PTY

    val tty : t -> Fd.t option
    (** The terminal-device end for attaching children, if the platform has one. *)

    val pty : t -> Fd.t option
    (** The pseudoterminal-device end, if the platform has a single one. *)
  end

  type (_, _, _) Eio.Resource.pi +=
    | Unix_pty : ('t, (module UNIX_PTY with type t = 't), [> Eio.Pty.pty_ty]) Eio.Resource.pi

  val unix_pty : (module UNIX_PTY with type t = 't) -> ('t, Eio.Pty.ty) Eio.Resource.handler
end

val open_posix : sw:Switch.t -> size:winsize -> Eio.Pty.ty r
(** [open_posix ~sw ~size] is the POSIX pseudoterminal implementation.

    Both file descriptors are closed when [sw] finishes. The {!pty} end is
    non-blocking and the {!tty} end is blocking and suitable as the child's
    controlling terminal. This is not a multi-domain-safe function on some platforms
    without reentrant [ptsname] support. *)
