(** Fallback Eio backend for Windows using OCaml's [Unix.select]. *)

type stdenv = Eio_unix.Stdenv.base
(** An extended version of {!Eio.Stdenv.base} with some extra features available on Windows. *)

val run : (stdenv -> 'a) -> 'a
(** [run main] runs an event loop and calls [main stdenv] inside it.

    For portable code, you should use {!Eio_main.run} instead, which will call this for you if appropriate. *)

module Low_level = Low_level
(** Low-level API. *)

module Path_syntax : sig
  (** Windows path syntax, for backends implementing {!Eio.Fs.Pi.DIR}. *)

  val is_step : string -> bool
  (** [is_step s] is [true] unless [s] is absolute, rooted ([\x]) or drive-relative ([C:x]). *)

  val join : string -> string -> string
  (** [join dir step] is {!Eio.Fs.Pi.DIR.join} for Windows syntax.
      It uses ["/"] as the separator, except in verbatim paths (which use
      ["\\"]) and after a bare drive (where no separator is added, to keep
      the path drive-relative). *)

  val split : string -> (string * string) option
  (** [split p] is {!Eio.Fs.Pi.DIR.split} for Windows syntax. *)
end
