open Nanomsg

(** {1 Asynchronous I/O} *)

(** {2 Zero-copy I/O} *)

val send : socket -> Lwt_bytes.t -> int -> int -> unit Lwt.t
val recv : socket -> (Lwt_bytes.t -> 'a Lwt.t) -> 'a Lwt.t
(** [recv sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val send_from_bytes : socket -> Bytes.t -> int -> int -> unit Lwt.t
val send_from_string_raw : socket -> string -> int -> int -> unit Lwt.t
val send_from_string : socket -> string -> unit Lwt.t
val recv_to_string : socket -> string Lwt.t
