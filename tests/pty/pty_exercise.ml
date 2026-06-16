(* A single, platform-identical pty lifecycle exercise. It opens a pty, resizes
   it, spawns a marker-printing child attached via [~login_tty], scans the
   master output for the marker, and prints the child's exit code. The transcript
   is the same on POSIX and Windows even though the child output framing differs
   (a POSIX pty adds \r; ConPTY wraps the payload in VT escapes), so we always
   substring-scan the accumulated buffer rather than compare exact bytes. *)

(* Force '\n' newlines so \r\n does not break the mdx comparison on Windows. *)
let () = set_binary_mode_out stdout true

open Eio.Std

module B = Pty_backend

let marker = "PTY_MARKER_73"

let say fmt = Printf.ksprintf (fun s -> print_string s; print_char '\n') fmt

(* Substring search over the accumulated (VT-noisy, \r-laden) output. *)
let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i = i + nl <= sl && (String.sub s i nl = needle || go (i + 1)) in
  go 0

(* Read chunks until [needle] appears. End_of_file (and EIO on POSIX when the
   child closes the slave) are treated as end-of-stream. This can block if the
   marker never arrives and the master never closes (ConPTY keeps it open past
   child exit), so the caller bounds it with a timeout. *)
let read_until ~needle src =
  let buf = Cstruct.create 4096 in
  let acc = Buffer.create 256 in
  let rec loop () =
    if contains ~needle (Buffer.contents acc) then true
    else
      match Eio.Flow.single_read src buf with
      | got ->
        Buffer.add_string acc (Cstruct.to_string (Cstruct.sub buf 0 got));
        loop ()
      | exception End_of_file -> contains ~needle (Buffer.contents acc)
      | exception Unix.Unix_error (Unix.EIO, _, _) ->
        contains ~needle (Buffer.contents acc)
  in
  loop ()

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let t = B.open_pty ~sw in
  say "open: ok";
  B.resize t ~rows:40 ~cols:100;
  let rows, cols = B.get_size t in
  say "resize: %dx%d" rows cols;
  let executable, args = B.child_command in
  let child =
    Eio_unix.Process.spawn_unix ~sw env#process_mgr
      ~login_tty:(B.login_tty t) ~fds:[] ~executable args
  in
  say "spawn: ok";
  let found =
    Fiber.first
      (fun () -> read_until ~needle:marker (B.source t))
      (fun () -> Eio.Time.sleep env#clock 10.; false)
  in
  say "marker: %s" (if found then "found" else "TIMEOUT");
  let code =
    match Eio.Process.await child with
    | `Exited n -> n
    | `Signaled n -> 128 + n
  in
  say "exit: %d" code;
  flush stdout
