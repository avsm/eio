# Windows path syntax

`Eio_utils.Nt_path` implements the path syntax used by the Windows backend, in
the same way that `Eio_utils.Posix_path` (tested in `fs.md`) does for the Unix
ones. It is just string manipulation, so it can be tested on any platform.

```ocaml
# #require "eio.utils";;
```

```ocaml
let split = Eio_utils.Nt_path.split
let join = Eio_utils.Nt_path.join
```

# Join

`join` puts a "\" between the two parts, unless the first already ends with a
separator (either kind):

```ocaml
# join "a" "b";;
- : string = "a\\b"

# join "a\\" "b";;
- : string = "a\\b"

# join "a/" "b";;
- : string = "a/b"

# join "a" "";;
- : string = "a\\"

# join "" "b";;
- : string = "b"

# join "." "b";;
- : string = "b"
```

A directory that is a root already ends in a separator, so nothing extra is
added. A bare drive is the exception: `C:x` names a file in the current
directory of drive C, whereas `C:\x` is at the drive's root, so adding a
separator would change the meaning:

```ocaml
# join "C:\\" "b";;
- : string = "C:\\b"

# join "C:" "x";;
- : string = "C:x"

# join "\\" "x";;
- : string = "\\x"

# join "\\\\srv\\share\\" "x";;
- : string = "\\\\srv\\share\\x"

# join "\\\\?\\C:\\" "b";;
- : string = "\\\\?\\C:\\b"

# join "\\??\\C:\\a" "b";;
- : string = "\\??\\C:\\a\\b"
```

A path that names its own root replaces the directory rather than extending it,
as an absolute path does in POSIX syntax. As well as the absolute form, that
covers the drive-relative (`C:x`) and rooted (`\x`) paths, which Windows
resolves without reference to our directory. Such a path will then be rejected
when it is opened, unless it is being used with `fs`:

```ocaml
# join "a" "C:\\b";;
- : string = "C:\\b"

# join "a" "C:/b";;
- : string = "C:/b"

# join "a" "C:b";;
- : string = "C:b"

# join "a" "C:";;
- : string = "C:"

# join "a" "\\b";;
- : string = "\\b"

# join "a" "/b";;
- : string = "/b"

# join "a" "\\\\srv\\share";;
- : string = "\\\\srv\\share"

# join "a" "\\\\?\\C:\\b";;
- : string = "\\\\?\\C:\\b"

# join "a" "\\??\\C:\\b";;
- : string = "\\??\\C:\\b"

# join "a" "\\\\.\\NUL";;
- : string = "\\\\.\\NUL"
```

# Split

Either separator ends a component, and trailing separators are ignored:

```ocaml
# split "a\\b";;
- : (string * string) option = Some ("a", "b")

# split "a/b\\c";;
- : (string * string) option = Some ("a/b", "c")

# split "a\\b\\\\";;
- : (string * string) option = Some ("a", "b")

# split "a";;
- : (string * string) option = Some ("", "a")

# split "";;
- : (string * string) option = None
```

The volume prefix is never split, and neither is the separator that follows it,
since removing that would change an absolute path into a drive-relative one:

```ocaml
# split "\\x";;
- : (string * string) option = Some ("\\", "x")

# split "\\";;
- : (string * string) option = None

# split "C:\\a\\b";;
- : (string * string) option = Some ("C:\\a", "b")

# split "C:\\b";;
- : (string * string) option = Some ("C:\\", "b")

# split "C:\\";;
- : (string * string) option = None

# split "C:x";;
- : (string * string) option = Some ("C:", "x")

# split "C:";;
- : (string * string) option = None

# split "\\\\srv\\share\\x";;
- : (string * string) option = Some ("\\\\srv\\share\\", "x")

# split "\\\\srv\\share";;
- : (string * string) option = None

# split "\\\\.\\NUL";;
- : (string * string) option = None
```

Win32 passes verbatim (`\\?\`) and NT (`\??\`) paths through without any
normalisation, so there "/" is an ordinary character rather than a separator.
The NT form needs backslashes for the same reason:

```ocaml
# split "\\\\?\\C:\\a/b";;
- : (string * string) option = Some ("\\\\?\\C:\\", "a/b")

# split "\\\\?\\UNC\\srv\\share\\x";;
- : (string * string) option = Some ("\\\\?\\UNC\\srv\\share\\", "x")

# split "\\??\\C:\\a\\b";;
- : (string * string) option = Some ("\\??\\C:\\a", "b")

# split "/??/C:/x";;
- : (string * string) option = Some ("/??/C:", "x")
```

# Round-tripping

Joining the two parts of a split gives back the path it came from:

```ocaml
# List.filter (fun p -> Option.map (fun (d, b) -> join d b) (split p) <> Some p) [
    "a\\b";
    "C:\\a\\b";
    "C:\\b";
    "C:x";
    "\\x";
    "\\\\srv\\share\\x";
    "\\\\?\\C:\\a\\b";
    "\\\\?\\C:\\a/b";
    "\\\\?\\UNC\\srv\\share\\x";
    "\\??\\C:\\a\\b";
  ];;
- : string list = []
```

Components separated by "/" come back separated by "\", which Win32 treats the
same way (outside of the verbatim and NT namespaces, where the separator was
never "/" to begin with):

```ocaml
# split "a/b" |> Option.map (fun (d, b) -> join d b);;
- : string option = Some "a\\b"
```
