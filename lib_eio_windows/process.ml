open Eio.Std

module Fd = Eio_unix.Fd

(* eio_spawn cwd env stdin stdout stderr cmdline
   Returns the child's pid and a handle to it. *)
external eio_spawn :
  Unix.file_descr option -> string ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  string -> int * Unix.file_descr
  = "caml_eio_windows_spawn_bytes" "caml_eio_windows_spawn"

external eio_process_wait : Unix.file_descr -> int = "caml_eio_windows_process_wait"
external eio_process_terminate : Unix.file_descr -> int -> unit = "caml_eio_windows_process_terminate"

(* One NUL-separated block with the double-NUL terminator CreateProcess
   expects, as the stdlib's [make_process_env] builds. *)
let make_env env =
  Array.iter
    (fun s ->
       if s = "" || String.contains s '\000' then
         Fmt.invalid_arg "spawn: invalid environment entry %S" s)
    env;
  String.concat "\000" (Array.to_list env) ^ "\000\000"

(* Follow the stdlib convention here of not quoting unnecessarily due to Windows
   cmd.exe behaving differently if quoted. *)
let command_line args =
  let quote_arg arg =
    if arg = "" || String.exists (function ' ' | '\t' | '\n' | '\011' | '"' -> true | _ -> false) arg
    then Filename.quote arg
    else arg in
  String.concat " " (List.map quote_arg args)

(* The exit code of a process we terminate: what Windows gives one ended by Ctrl-C. *)
let terminated_exit_code = 0xC000013A

module Process = struct
  type t = {
    pid : int;
    handle : Fd.t;
    mutable signalled : int option;
    mutable status : Eio.Process.exit_status option;
  }

  let pid t = t.pid

  let await t =
    match t.status with
    | Some status -> status
    | None ->
      (* The wait outlives a cancelled await, but not the process: [stop] reaps it. *)
      let code =
        Fd.use_exn "process_wait" t.handle @@ fun h ->
        Sched.enter @@ fun sched k -> Sched.await_thread sched k (fun () -> eio_process_wait h)
      in
      let status =
        match t.signalled with
        | Some signum when code = terminated_exit_code -> `Signaled signum
        | _ -> `Exited code
      in
      t.status <- Some status;
      status

  (* Windows has no signals: any signal terminates the process. *)
  let signal t signum =
    Fd.use t.handle ~if_closed:ignore (fun h ->
        t.signalled <- Some signum;
        eio_process_terminate h terminated_exit_code)

  let stop t =
    Eio.Cancel.protect @@ fun () ->
    signal t Sys.sigkill;
    ignore (await t : Eio.Process.exit_status)
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

    (* The directory is opened through its sandbox and the child is given the path of the handle. *)
    let with_cwd cwd fn =
      match cwd with
      | None -> fn None
      | Some ((dir, path) : Eio.Fs.dir_ty Eio.Path.t) ->
        match Fs.Handler.as_posix_dir dir with
        | None -> Fmt.invalid_arg "cwd is not an eio_windows directory!"
        | Some d ->
          Switch.run ~name:"spawn_unix" @@ fun sw ->
          let open Low_level in
          let flags = Flags.Open.(generic_read + synchronise) in
          let dirfd =
            Err.run (openat ~sw ~follow:Nofollow (Fs.Dir.resolve d path) flags Flags.Disposition.open_)
              Flags.Create.directory
          in
          Fd.use_exn "cwd" dirfd (fun h -> fn (Some h))

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
      (* The executable heads the command line: it becomes the child's argv[0]
         and CreateProcess resolves it with its own search. *)
      let cmdline =
        command_line (executable :: (match args with [] -> [] | _ :: tl -> tl))
      in
      let env = make_env env in
      let find n = List.find_map (fun (i, fd, _) -> if i = n then Some fd else None) fds in
      let get n name =
        match find n with
        | Some fd -> fd
        | None -> Fmt.invalid_arg "spawn: no file descriptor for %s (fd %d)" name n
      in
      let stdin_fd = get 0 "stdin" and stdout_fd = get 1 "stdout" and stderr_fd = get 2 "stderr" in
      let pid, raw_handle =
        with_cwd cwd @@ fun cwd ->
        Fd.use_exn "stdin" stdin_fd @@ fun h0 ->
        Fd.use_exn "stdout" stdout_fd @@ fun h1 ->
        Fd.use_exn "stderr" stderr_fd @@ fun h2 ->
        eio_spawn cwd env h0 h1 h2 cmdline
      in
      let handle = Fd.of_unix ~sw ~blocking:true ~close_unix:true raw_handle in
      let t = { Process.pid; handle; signalled = None; status = None } in
      Switch.on_release sw (fun () -> Process.stop t);
      process t
  end

  include Eio_unix.Process.Make_mgr (T)
end

let mgr : Eio_unix.Process.mgr_ty r =
  let h = Eio_unix.Process.Pi.mgr_unix (module Impl) in
  Eio.Resource.T ((), h)
