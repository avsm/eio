(** Windows path syntax.

    - [join dir p] is just [p] if [p] is rooted, i.e. absolute ([C:\x]), rooted on
      the current drive ([\x]) or drive-relative ([C:x]). Otherwise it appends [p]
      to [dir] using ["\\"] as the separator, unless [dir] already ends in one.
      Nothing is added after a bare drive ([C:]), as that would turn a
      drive-relative path into an absolute one.

    - [split] never splits a volume prefix ([C:], [\\server\share], [\\?\...] or
      [\??\...]) and ignores trailing separators, keeping just the one that
      separates a root from its contents.

    Both accept ["/"] as a separator, except in the verbatim ([\\?\]) and NT
    ([\??\]) namespaces, where Win32 does no normalisation and only ["\\"]
    separates. *)

include Eio.Fs.Pi.PATH
