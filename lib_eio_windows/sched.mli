(** The scheduler keeps track of all suspended fibers and resumes them as appropriate.

    Each Eio domain has one scheduler, which keeps a queue of runnable
    processes plus a record of all fibers waiting for IO operations to complete. *)

type t

val socketpair : Unix.socket_domain -> Unix.socket_type -> int -> Unix.file_descr * Unix.file_descr
(** Like {!Unix.socketpair} (close-on-exec). For AF_UNIX stream pairs it emulates
    the pair via a listener bound to a per-process-unique address, avoiding the
    cross-process [EADDRINUSE]/[EACCES] collisions of OCaml's own Windows AF_UNIX
    socketpair emulation. *)

type exit
(** This is equivalent to [unit], but indicates that a function returning this will call {!next}
    and so does not return until the whole event loop is finished. Such functions should normally
    be called in tail position. *)

val with_sched : (t -> 'a) -> 'a
(** [with_sched fn] sets up a fresh scheduler [t] and calls [fn t]
    (typically [fn] will call {!run}). The scheduler holds a completion port that
    is reclaimed by a GC finaliser (there is no explicit close), and a systhread
    pool that is torn down by {!run}; neither is released synchronously when [fn]
    returns. *)

val run :
  extra_effects:exit Effect.Deep.effect_handler ->
  t -> ('a -> 'b) -> 'a -> 'b [@@alert "-unstable"]
(** [run ~extra_effects t f x] starts an event loop using [t] and runs [f x] as the root fiber within it.

    Unknown effects are passed to [extra_effects]. *)

val next : t -> exit
(** [next t] asks the scheduler to transfer control to the next runnable fiber,
    or wait for an event from the OS if there is none. This should normally be
    called in tail position from an effect handler. *)

val get : unit -> t
(** [get ()] returns the scheduler running the current fiber. Used to associate
    freshly-created FDs with the completion port. *)

val associate : t -> Unix.file_descr -> unit
(** [associate t fd] registers [fd] with [t]'s completion port so that overlapped
    operations on it deliver completions to this domain. Idempotent. *)

val iocp : t -> Iocp.t
(** [iocp t] is [t]'s completion port, used by the synchronous socket helpers that
    aren't themselves overlapped: updating an accepted or connected socket's
    context after AcceptEx/ConnectEx. *)

val cancel_reads : t -> Unix.file_descr -> unit
(** [cancel_reads t fd] cancels every in-flight overlapped read on [fd] (those
    submitted with [enter_io ~read:fd]), leaving concurrent writes on the same
    handle untouched. Used by a receive-side [shutdown] to wake a blocked reader:
    each cancelled recv completes with [ERROR_OPERATION_ABORTED], which the read
    path reports as end-of-file. *)

val enter_io : ?read:Unix.file_descr -> string -> (Iocp.t -> Iocp.Id.t option) -> Iocp.completion_status
(** [enter_io op submit] starts the overlapped operation [submit t.iocp] and
    suspends the calling fiber until it completes, returning its completion
    status. [op] labels the operation for tracing. Backpressure (an exhausted
    OVERLAPPED pool) and cancellation are handled transparently. [read] passes the
    raw handle when the operation is a recv, so it can be cancelled by
    {!cancel_reads} on a receive-side shutdown. *)

val await_readable : t -> unit Eio_utils.Suspended.t -> Unix.file_descr -> exit
(** [await_readable t k fd] resumes [k] once [fd] is readable, emulated by a
    zero-byte overlapped recv (sockets only). *)

val await_writable : t -> unit Eio_utils.Suspended.t -> Unix.file_descr -> exit
(** [await_writable t k fd] resumes [k] once [fd] is writable, emulated by a
    zero-byte overlapped send (sockets only). *)

val await_timeout : t -> unit Eio_utils.Suspended.t -> Mtime.t -> exit
(** [await_timeout t k time] adds [time, k] to the timer.

    When [time] is reached, [k] is resumed. Cancelling [k] removes the entry from the timer. *)

val enter : string -> (t -> 'a Eio_utils.Suspended.t -> exit) -> 'a
(** [enter op fn] suspends the current fiber (labelling it [op] for tracing) and
    runs [fn t k] in the scheduler's context.

    [fn] should either resume [k] immediately itself, or call one of the [await_*] functions above. *)
