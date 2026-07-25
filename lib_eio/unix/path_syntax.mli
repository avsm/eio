(** POSIX path syntax. *)

val is_step : string -> bool
(** [is_step s] is [true] unless [s] begins with ["/"]. *)

val join : string -> string -> string
(** [join dir step] is {!Eio.Fs.Pi.DIR.join} for POSIX syntax: like
    [Filename.concat], but always using ["/"] as the separator. *)

val split : string -> (string * string) option
(** [split p] is {!Eio.Fs.Pi.DIR.split} for POSIX syntax. *)
