(** Windows path syntax.

    - [join dir step] appends [step] to [dir] using ["\\"] as the directory separator,
      unless [dir] already ends in a separator. After a bare drive ([C:]) no
      separator is added, to keep the path drive-relative.

    - [split]: Volume prefixes ([C:], [\\server\share], [\\?\...], [\??\...]) are never split. *)

include Eio.Fs.Pi.PATH

val ( / ) : string -> string -> string
(** [a / b] is [join a b]. *)

val is_relative : string -> bool
(** [is_relative p] is [true] unless [p] begins with a volume or a separator.
    A drive-relative path such as [C:x] is not relative, since it is resolved
    against that drive's own current directory rather than ours. *)

val dirname : string -> string
(** [dirname p] is the directory part of [p]. It is ["."] when [p] names
    something in the current directory, and also when [p] has no parent to
    name (e.g. the empty path, a bare volume ([C:]) and a root ([\\server\share])). *)

val basename : string -> string
(** [basename p] is the final component of [p]. It is [p] itself when [p] has
    no directory part, and ["."] when [p] is empty. *)

val to_nt : cwd:string -> string -> string
(** [to_nt ~cwd path] is the NT object-manager form of the Win32 path [path].

    A relative [path] is resolved against [cwd] and, as in Win32, ["/"] is a
    separator and ["."] and [".."] components are removed. Verbatim ([\\?\])
    and NT ([\??\]) paths are passed through unchanged. *)

val link_target : cwd:string -> string -> string
(** [link_target ~cwd p] is the name Windows stores in a symlink to [p]:
    a relative [p] keeps its form, with ["\\"] separators, and an absolute
    one is [to_nt ~cwd p]. *)
