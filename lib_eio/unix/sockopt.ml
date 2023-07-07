type socket_int_option =
    EIO_TCP_CORK
  | EIO_TCP_KEEPCNT
  | EIO_TCP_KEEPIDLE
  | EIO_TCP_KEEPINTVL

external setsockopt_int : Unix.file_descr -> socket_int_option -> int -> unit =
  "eio_unix_setsockopt_int"
external getsockopt_int : Unix.file_descr -> socket_int_option -> int =
  "eio_unix_getsockopt_int"

type _ t =
  | SO_KEEPALIVE : bool t
  | SO_REUSEADDR : bool t
  | SO_REUSEPORT : bool t
  | TCP_CORK : int t
  | TCP_KEEPCNT : int t
  | TCP_KEEPIDLE : int t
  | TCP_KEEPINTVL : int t

let set : type a . Fd.t -> a t -> a -> unit = fun sock k v ->
  Fd.use_exn "Sockaddr.set" sock @@ fun fd ->
  match k with
  | TCP_CORK -> setsockopt_int fd EIO_TCP_CORK v
  | TCP_KEEPCNT -> setsockopt_int fd EIO_TCP_KEEPCNT v
  | TCP_KEEPIDLE -> setsockopt_int fd EIO_TCP_KEEPIDLE v
  | TCP_KEEPINTVL -> setsockopt_int fd EIO_TCP_KEEPINTVL v
  | SO_KEEPALIVE -> Unix.(setsockopt fd SO_KEEPALIVE v)
  | SO_REUSEADDR -> Unix.(setsockopt fd SO_REUSEADDR v)
  | SO_REUSEPORT -> Unix.(setsockopt fd SO_REUSEPORT v)

let get_descr : type a . Unix.file_descr -> a t -> a = fun fd k ->
  match k with
  | TCP_CORK -> getsockopt_int fd EIO_TCP_CORK
  | TCP_KEEPCNT -> getsockopt_int fd EIO_TCP_KEEPCNT
  | TCP_KEEPIDLE -> getsockopt_int fd EIO_TCP_KEEPIDLE
  | TCP_KEEPINTVL -> getsockopt_int fd EIO_TCP_KEEPINTVL
  | SO_KEEPALIVE -> Unix.(getsockopt fd SO_KEEPALIVE)
  | SO_REUSEADDR -> Unix.(getsockopt fd SO_REUSEADDR)
  | SO_REUSEPORT -> Unix.(getsockopt fd SO_REUSEPORT)

let get : type a . Fd.t -> a t -> a = fun sock k ->
  Fd.use_exn "Sockaddr.get" sock (fun fd -> get_descr fd k)
