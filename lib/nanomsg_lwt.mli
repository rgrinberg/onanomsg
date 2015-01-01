open Nanomsg

(** {1 Asynchronous I/O} *)

(** {2 Zero-copy I/O} *)

val send_bigstring : socket -> CCBigstring.t -> unit Lwt.t
val send_bigstring_buf : socket -> CCBigstring.t -> int -> int -> unit Lwt.t

val send_string : socket -> string -> unit Lwt.t
val send_string_buf : socket -> string -> int -> int -> unit Lwt.t

val send_bytes : socket -> Bytes.t -> unit Lwt.t
val send_bytes_buf : socket -> Bytes.t -> int -> int -> unit Lwt.t

val recv : socket -> (CCBigstring.t -> 'a Lwt.t) -> 'a Lwt.t
(** [recv sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : socket -> string Lwt.t
val recv_bytes : socket -> Bytes.t Lwt.t
val recv_bytes_buf : socket -> Bytes.t -> int -> unit Lwt.t
