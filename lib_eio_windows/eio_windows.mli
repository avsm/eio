(** Fallback Eio backend for Windows using I/O completion ports (IOCP). *)

type stdenv = Eio_unix.Stdenv.base
(** The standard environment provided to {!run}'s callback (see {!Eio_unix.Stdenv.base}). *)

val run : (stdenv -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate. *)

module Low_level = Low_level
(** Low-level API. *)

module Pty : sig
  (** Pseudoterminal support via the Windows Pseudo Console API (ConPTY,
      Windows 10 1809+).

      This mirrors the shape of {!Eio_unix.Pty} so call sites can be ported
      mechanically, but the semantics differ from POSIX ptys:

      - The child's output on the master is {b VT/ANSI-encoded} by the console
        (cursor-positioning and other escape sequences surround the payload), so
        readers must not assume raw byte-for-byte echo.
      - There is no [SIGWINCH]: a child observes {!resize} through the console
        APIs, not a signal.
      - There is no session or controlling-terminal concept. Here [login_tty]
        means only "attach to this pseudoconsole"; process-group operations
        remain unsupported.
      - {b Close order}: closing the pseudoconsole tears the console down (the
        child sees it disappear), so on switch release the [HPCON] is closed
        first and the master last, letting buffered child output still drain.
        The supported shutdown pattern is to close the {!sink} and then [await]
        the child's exit.

      On a system without ConPTY, {!open_pty} raises [Unix.Unix_error] with
      [EOPNOTSUPP]. *)

  open Eio.Std

  type t
  (** A pseudoterminal: two pipe pairs (child input and output) paired with a
      pseudoconsole.

      The design's single duplex-named-pipe master proved unworkable — ConPTY
      emitted only its init sequence and never rendered child output through a
      handle shared as both its input and output — so two separate pipes are
      used. As a result the master is read-only ({!pty}/{!source}) and child
      input goes through a distinct {!sink}, rather than one bidirectional fd. *)

  (** Terminal window dimensions. *)
  type winsize = {
    rows : int;     (** Height of the terminal in character rows. *)
    cols : int;     (** Width of the terminal in character columns. *)
    xpixel : int;   (** Width in pixels ([0] if unknown). *)
    ypixel : int;   (** Height in pixels ([0] if unknown). *)
  }

  val open_pty : sw:Switch.t -> ?size:winsize -> unit -> t
  (** [open_pty ~sw ?size ()] allocates a new pseudoterminal.

      [size] defaults to 80 columns by 24 rows (ConPTY requires a size at
      creation). The master and tty handles are closed and the pseudoconsole
      destroyed when [sw] finishes (see the close-order note above).

      @raise Unix.Unix_error [EOPNOTSUPP] if ConPTY is unavailable. *)

  val pty : t -> Eio_unix.Fd.t
  (** [pty t] is the master end: the read side of the output pipe, from which the
      application reads the child's (VT-encoded) output. Unlike a POSIX pty
      controller it is read-only; write child input through {!sink}. *)

  val tty : t -> Eio_unix.Fd.t
  (** [tty t] is the terminal token. Pass it as [~login_tty] to
      {!Eio_unix.Process.spawn_unix} to attach a child to this pseudoconsole. It
      is an identity handle only, not for I/O. *)

  val name : t -> string
  (** [name t] is a synthetic identifier for the pty (the backing pipes are
      anonymous and have no OS path). *)

  val source : t -> Eio_unix.source_ty r
  (** [source t] reads the output the child writes to the terminal. *)

  val sink : t -> Eio_unix.sink_ty r
  (** [sink t] writes input for the child to read from its terminal. *)

  val resize : t -> winsize -> unit
  (** [resize t size] resizes the pseudoconsole and records [size] as the current
      window size. *)

  val get_window_size : t -> winsize
  (** [get_window_size t] returns the size last passed to {!open_pty} or
      {!resize}. ConPTY has no size-query call; this stored value is
      authoritative because the application is the only resizer. *)
end
