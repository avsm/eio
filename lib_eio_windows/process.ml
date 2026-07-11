open Eio.Std

module Fd = Eio_unix.Fd

external eio_spawn :
  string -> string array -> string ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  string -> int * Unix.file_descr
  = "caml_eio_windows_spawn_bytes" "caml_eio_windows_spawn"

external eio_process_wait : Unix.file_descr -> int = "caml_eio_windows_process_wait"
external eio_process_terminate : Unix.file_descr -> int -> unit = "caml_eio_windows_process_terminate"

(* Follow the stdlib convention here of not quoting unnecessarily due to Windows
   cmd.exe behaving differently if quoted. *)
let command_line args =
  let quote_arg arg =
    if arg = "" || String.exists (function ' ' | '\t' | '\n' | '\011' | '"' -> true | _ -> false) arg
    then Filename.quote arg
    else arg in
  String.concat " " (List.map quote_arg args)

module Process = struct
  type t = {
    pid : int;
    handle : Fd.t;
    mutable status : int option;
    lock : Eio.Mutex.t;
  }

  let pid t = t.pid
  (* Terminated processes report this exit code *)
  let terminate_exit_code = 1

  let exit_code t =
    Eio.Cancel.protect @@ fun () ->
    Eio.Mutex.lock t.lock;
    Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.lock) @@ fun () ->
    match t.status with
    | Some c -> c
    | None ->
      let c =
        Eio_unix.run_in_systhread ~label:"process_wait" (fun () ->
            Fd.use t.handle eio_process_wait
              ~if_closed:(fun () ->
                  failwith "process_wait: handle closed before the process was reaped"))
      in
      t.status <- Some c;
      c

  let await t : Eio.Process.exit_status =
    `Exited (exit_code t)

  let signal t (_signum : int) =
    (* TODO avsm: all signals are kills here *)
    Fd.use t.handle ~if_closed:ignore (fun h -> eio_process_terminate h terminate_exit_code)

  let stop t =
    signal t Sys.sigkill;
    ignore (exit_code t : int)
end

module Process_impl = struct
  type t = Process.t
  type tag = [ `Generic | `Unix ]

  let pid = Process.pid
  let await = Process.await
  let signal = Process.signal
end

let process_handler = Eio.Process.Pi.process (module Process_impl)
let process t = Eio.Resource.T (t, process_handler)

module Impl = struct
  module T = struct
    type t = unit

    let spawn_unix () ~sw ?cwd ?pgid ?uid ?gid ?login_tty ~env ~fds ~executable args =
      if pgid <> None || uid <> None || gid <> None then
        Fmt.invalid_arg "spawn: pgid/uid/gid are not supported on Windows";
      (* An arbitrary fd cannot be a controlling terminal on Windows. *)
      if login_tty <> None then
        Fmt.invalid_arg "spawn: login_tty is not supported on Windows";
      (* Windows has no per-fd inheritance table so only the three standard streams *)
      List.iter (fun (i, _, _) ->
          if i > 2 then Fmt.invalid_arg "spawn: only fds 0-2 are supported on Windows (got fd %d)" i)
        fds;
      let cwd =
        match cwd with
        | None -> ""
        | Some ((dir, p) : Eio.Fs.dir_ty Eio.Path.t) ->
          begin match Fs.Handler.as_posix_dir dir with
            | None -> Fmt.invalid_arg "cwd is not an eio_windows directory!"
            | Some d -> Fs.Dir.strip_nt_prefix (Err.run (Fs.Dir.resolve d) p)
          end
      in
      let cmdline = command_line args in
      let find n = List.find_map (fun (i, fd, _) -> if i = n then Some fd else None) fds in
      let get n name =
        match find n with
        | Some fd -> fd
        | None -> Fmt.invalid_arg "spawn: no file descriptor for %s (fd %d)" name n
      in
      let stdin_fd = get 0 "stdin" and stdout_fd = get 1 "stdout" and stderr_fd = get 2 "stderr" in
      let pid, raw_handle =
        Fd.use_exn "stdin" stdin_fd @@ fun h0 ->
        Fd.use_exn "stdout" stdout_fd @@ fun h1 ->
        Fd.use_exn "stderr" stderr_fd @@ fun h2 ->
        eio_spawn cwd env executable h0 h1 h2 cmdline
      in
      let handle = Fd.of_unix ~sw ~blocking:true ~close_unix:true raw_handle in
      let t = { Process.pid; handle; status = None; lock = Eio.Mutex.create () } in
      Switch.on_release sw (fun () -> Process.stop t);
      process t
  end

  include Eio_unix.Process.Make_mgr (T)
end

let mgr : Eio_unix.Process.mgr_ty r =
  let h = Eio_unix.Process.Pi.mgr_unix (module Impl) in
  Eio.Resource.T ((), h)
