(** Platform seam for the shared pty exercise.

    Two implementations ([pty_backend.win32.ml] over {!Eio_windows.Pty} and
    [pty_backend.posix.ml] over {!Eio_unix.Pty}) are selected by dune, so the
    executable in [pty_exercise.ml] runs unchanged on both platforms. *)

open Eio.Std

type t
(** An open pseudoterminal. *)

val open_pty : sw:Switch.t -> t
(** [open_pty ~sw] allocates a pseudoterminal whose handles close when [sw]
    finishes. *)

val login_tty : t -> Eio_unix.Fd.t
(** [login_tty t] is the terminal token to pass as [~login_tty] when spawning a
    child attached to this pty. *)

val source : t -> Eio_unix.source_ty r
(** [source t] reads what the child writes to the terminal. *)

val resize : t -> rows:int -> cols:int -> unit
(** [resize t ~rows ~cols] sets the terminal window size. *)

val get_size : t -> int * int
(** [get_size t] reads the window size back as [(rows, cols)]. *)

val child_command : string * string list
(** [(executable, args)] for a child that prints the marker string, using an
    absolute executable path (the portable spawn's PATH resolution is
    POSIX-only). *)
