open Eio.Std

type tristate = No | Yes | Unknown

(* Note: [blocking] and [seekable] are not atomic,
   but it doesn't matter if we query twice in rare cases. *)
type t = {
  fd : Rcfd.t;
  mutable blocking : tristate;
  mutable seekable : tristate;
  ownership : Fd_ownership.t;
  mutable release_hook : Eio.Switch.hook;     (* Use this on close to remove switch's [on_release] hook. *)
}

let err_closed op = Invalid_argument (op ^ ": file descriptor used after calling close!")

let use t f ~if_closed = Rcfd.use t.fd f ~if_closed

let use_exn op t f =
  Rcfd.use t.fd f ~if_closed:(fun () -> raise (err_closed op))

let close t =
  Switch.remove_hook t.release_hook;
  Fd_ownership.close t.ownership t.fd

let remove t =
  Switch.remove_hook t.release_hook;
  Fd_ownership.remove t.ownership t.fd

module Windows = struct
  let id t = Fd_ownership.id t.ownership
  let set_detach t fn = Fd_ownership.set_detach t.ownership fn
end

let tristate_of_bool_opt = function
  | None -> Unknown
  | Some true -> Yes
  | Some false -> No

let of_unix_no_hook ?(close_unix=true) ?blocking ?seekable fd =
  let seekable = tristate_of_bool_opt seekable in
  let blocking = tristate_of_bool_opt blocking in
  { fd = Rcfd.make fd; blocking; seekable; ownership = Fd_ownership.make ~close_unix; release_hook = Switch.null_hook }

let of_unix ~sw ?blocking ?seekable ~close_unix fd =
  let t = of_unix_no_hook ?blocking ?seekable ~close_unix fd in
  t.release_hook <- Switch.on_release_cancellable sw (fun () -> close t);
  t

let of_unix_list ~sw fds =
  match Switch.get_error sw with
  | Some e -> List.iter Unix.close fds; raise e
  | None -> List.map (of_unix ~sw ~close_unix:true) fds

external eio_is_blocking : Unix.file_descr -> bool = "eio_unix_is_blocking"

let is_blocking t =
  match t.blocking with
  | No -> false
  | Yes -> true
  | Unknown ->
    use t ~if_closed:(Fun.const false) @@ fun fd ->
    let blocking = eio_is_blocking fd in
    t.blocking <- if blocking then Yes else No;
    blocking

let is_seekable t =
  match t.seekable with
  | No -> false
  | Yes -> true
  | Unknown ->
    use t ~if_closed:(Fun.const false) @@ fun fd ->
    let seekable =
      match Unix.lseek fd 0 Unix.SEEK_CUR with
      | (_ : int) -> true
      | exception Unix.Unix_error (Unix.ESPIPE, "lseek", "") -> false
    in
    t.seekable <- if seekable then Yes else No;
    seekable

let is_open t = Rcfd.is_open t.fd

let rec use_exn_list op xs k =
  match xs with
  | [] -> k []
  | x :: xs ->
    use_exn op x @@ fun x ->
    use_exn_list op xs @@ fun xs ->
    k (x :: xs)

let use_exn_opt op x f =
  match x with
  | None -> f None
  | Some x -> use_exn op x (fun x -> f (Some x))

let stdin = of_unix_no_hook Unix.stdin
let stdout = of_unix_no_hook Unix.stdout
let stderr= of_unix_no_hook Unix.stderr

let pp f t = Rcfd.pp f t.fd
