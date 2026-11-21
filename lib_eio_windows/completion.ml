(** Each scheduler consumes its own port. Registrations can outlive the
    scheduler and still deliver requests to the submitting scheduler. *)

type t = {
  port : Iocp.completion Eio.Promise.u Iocp.t;
  mutable users : int;
}

type _ Effect.t += Get : t Effect.t
let current () = Effect.perform Get

let dispatch = function
  | Iocp.Completion (resolver, c) -> Eio.Promise.resolve resolver c
  | Iocp.Wake -> ()

module Handles = Hashtbl.Make(struct
    type t = Eio_unix.Fd.t
    let equal = ( == )
    let hash = Eio_unix.Private.Windows_fd.id
  end)

let handles = Handles.create 16
let handles_lock = Mutex.create ()

let release t =
  t.users <- t.users - 1;
  if t.users = 0 then Iocp.close t.port

let with_port fn =
  let t = { port = Iocp.create ~skip_on_success:true (); users = 1 } in
  Fun.protect ~finally:(fun () -> Mutex.protect handles_lock (fun () -> release t)) (fun () -> fn t)

let register preferred raw =
  try preferred, Iocp.register preferred.port raw with
  | Unix.Unix_error (EINVAL, _, _) as ex ->
    (* A duplicate can already belong to another scheduler's port. *)
    match Handles.to_seq_values handles |> Seq.find_map (fun (owner, fd, _) ->
        if owner != preferred && Iocp.same_handle fd raw then Some owner else None) with
    | None -> raise ex
    | Some owner -> owner, Iocp.register owner.port raw

(** Borrow a registration inside [Fd.use]. Keep its port alive until the last
    descriptor user releases it, including on ownership transfer. *)
let registration fd raw =
  let preferred = current () in
  Mutex.protect handles_lock (fun () ->
      match Handles.find_opt handles fd with
      | Some entry -> entry
      | None ->
        let owner, h = register preferred raw in
        let entry = owner, raw, h in
        try
          Handles.add handles fd entry;
          Eio_unix.Private.Windows_fd.set_detach fd (fun () ->
              Mutex.protect handles_lock (fun () ->
                  Handles.remove handles fd;
                  Fun.protect ~finally:(fun () -> release owner) (fun () -> Iocp.close_handle h)));
          owner.users <- owner.users + 1;
          entry
        with ex ->
          Handles.remove handles fd;
          Iocp.close_handle h;
          raise ex)

let handle fd raw = let _, _, h = registration fd raw in h

let with_handle raw fn =
  let h = Iocp.register (current ()).port raw in
  Fun.protect ~finally:(fun () -> Iocp.close_handle h) (fun () -> fn h)

(** Submit an operation and wait for its completion. Cancellation drains the
    operation before releasing its resources. Call [on_complete] after draining,
    including on cancellation, before propagating any error. The caller must
    keep the handle open and the buffer valid until this function returns. *)
let await_result ?(on_complete=ignore) submit =
  Eio.Fiber.check ();
  let result, resolver = Eio.Promise.create () in
  let port = (current ()).port in
  let request = submit port ~data:resolver in
  Option.iter (Eio.Promise.resolve resolver) (Iocp.result request);
  match (if Eio.Promise.is_resolved result then Eio.Fiber.yield (); Eio.Promise.await result) with
  | c -> on_complete c; c
  | exception ex ->
    let bt = Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
        let ex, bt =
          try Iocp.cancel port request; ex, bt
          with ex -> ex, Printexc.get_raw_backtrace ()
        in
        let c = Eio.Promise.await result in
        on_complete c;
        (* A successful or truncated datagram can have transferred zero bytes. *)
        if c.error = None || c.bytes_transferred > 0 || Option.is_some c.source then c
        else Printexc.raise_with_backtrace ex bt)

let await ?on_complete submit =
  let c = await_result ?on_complete submit in
  match c.error with
  | Some code when c.bytes_transferred = 0 -> raise (Unix.Unix_error (code, "iocp", ""))
  | _ -> c.bytes_transferred

(** Shut down the socket and cancel receives when shutting down input. *)
let shutdown fd raw cmd =
  let owner, _, h = registration fd raw in
  Iocp.shutdown owner.port h cmd
