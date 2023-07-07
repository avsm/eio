type _ t =
 | SO_KEEPALIVE : bool t
 | SO_REUSEADDR : bool t
 | SO_REUSEPORT : bool t
 | TCP_CORK : int t
 | TCP_KEEPCNT : int t
 | TCP_KEEPIDLE : int t
 | TCP_KEEPINTVL : int t

val set : Fd.t -> 'a t -> 'a -> unit
val get : Fd.t -> 'a t -> 'a
