open Nanomsg

exception Error of string * string

val wrap_error : ('a, error) Result.result -> 'a Lwt.t
val bind_error : ('a -> 'b Lwt.t) -> ('a, error) Result.result -> 'b Lwt.t
val map_error : ('a -> 'b) -> ('a, error) Result.result -> 'b Lwt.t

(** {1 Asynchronous I/O} *)

(** {2 Zero-copy I/O} *)

val send_bigstring : ?pos:int -> ?len:int -> socket -> Bigstring.t -> unit Lwt.t
val send_string : ?pos:int -> ?len:int -> socket -> string -> unit Lwt.t
val send_bytes : ?pos:int -> ?len:int -> socket -> Bytes.t -> unit Lwt.t

val recv : socket -> (Bigstring.t -> 'a Lwt.t) -> 'a Lwt.t
(** [recv sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_buf : ?pos:int -> socket -> Bytes.t -> int Lwt.t
val recv_string : socket -> string Lwt.t
val recv_bytes : socket -> Bytes.t Lwt.t
