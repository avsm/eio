# Cross-platform pseudoterminal exercise

`pty_exercise.exe` drives one pty lifecycle — open, resize, spawn a child
attached via `~login_tty`, scan the child's output for a marker, and report the
exit code — through a dune-selected backend (`Eio_windows.Pty` on Windows,
`Eio_unix.Pty` on POSIX). The transcript is identical on both platforms.

The binary is run through the mdx toplevel rather than a cram block: cram needs
`sh` on `PATH`, which a PowerShell-launched dune does not have on Windows.

```ocaml
# Sys.command (Filename.concat Filename.current_dir_name "pty_exercise.exe");;
open: ok
resize: 40x100
spawn: ok
marker: found
exit: 0
- : int = 0
```
