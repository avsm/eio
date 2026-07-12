# Cross-platform pseudoterminal tests

`pty_test.exe` drives one pty lifecycle through the portable API.
The child commands differ per platform, and so only the transcript
is identical everywhere.

```ocaml
# Sys.command (Filename.concat Filename.current_dir_name "pty_test.exe");;
open: ok
size: 24x80
resize: 40x100
name: ok
spawn: ok
marker: found
exit: 0
input: reply found
interrupt: ok
checks: ok
- : int = 0
```
