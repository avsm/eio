(** Fallback Eio backend for Windows using OCaml's [Unix.select]. *)

type stdenv = Eio_unix.Stdenv.base
(** An extended version of {!Eio.Stdenv.base} with some extra features available on Windows. *)

val run : (stdenv -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate. *)

module Low_level = Low_level
(** Low-level API. *)

module Pty : sig
  (** Pseudoterminal support via the Windows Pseudo Console API. *)

  open Eio.Std

  type t
  (** Two pseudoterminal pipe pairs (input and output) *)

  (** Terminal window dimensions. *)
  type winsize = {
    rows : int;     (** Height of the terminal in character rows. *)
    cols : int;     (** Width of the terminal in character columns. *)
    xpixel : int;   (** Width in pixels ([0] if unknown). *)
    ypixel : int;   (** Height in pixels ([0] if unknown). *)
  }

  val open_pty : sw:Switch.t -> ?size:winsize -> unit -> t
  (** [open_pty ~sw ?size ()] allocates a new pseudoterminal.
      [size] defaults to 80 columns by 24 rows. 

      @raise Unix.Unix_error [EOPNOTSUPP] if ConPTY is unavailable. *)

  val pty : t -> Eio_unix.Fd.t
  (** [pty t] is the read side of the output pipe to obtain the
      child's output. *)

  val tty : t -> Eio_unix.Fd.t
  (** [tty t] is the terminal token to be passed to {!Eio_unix.Process.spawn_unix}
      to attach a child to this pseudoconsole. It is an identity handle only and not for I/O. *)

  val name : t -> string
  (** [name t] is a synthetic identifier for the pty (the backing pipes are
      anonymous and have no OS path). *)

  val source : t -> Eio_unix.source_ty r
  (** [source t] reads the output the child writes to the terminal. *)

  val sink : t -> Eio_unix.sink_ty r
  (** [sink t] writes input for the child to read from its terminal. *)

  val resize : t -> winsize -> unit
  (** [resize t size] resizes the pseudoconsole. *)

  val get_window_size : t -> winsize
  (** [get_window_size t] returns the size. *)
end
