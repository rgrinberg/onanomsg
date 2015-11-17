type domain =
  | AF_SP
  | AF_SP_RAW
  [@@deriving show]

type proto =
  | Pair
  | Pub
  | Sub
  | Req
  | Rep
  | Push
  | Pull
  | Surveyor
  | Respondent
  | Bus
  [@@deriving show]

type socket

module Addr : sig
  type bind = [
    | `All
    | `V4 of Ipaddr.V4.t
    | `V6 of Ipaddr.V6.t
    | `Iface of string ]
    [@@deriving show]

  type connect =
    [`V4 of Ipaddr.V4.t | `V6 of Ipaddr.V6.t | `Dns of string] *
    [`V4 of Ipaddr.V4.t | `V6 of Ipaddr.V6.t | `Iface of string] option
    [@@deriving show]

  type 'a t = [
    | `Inproc of string
    | `Ipc of string
    | `Tcp of 'a * int
  ] [@@deriving show]

  val bind_of_string : string -> bind t
  val bind_to_string : bind t -> string
  val connect_of_string : string -> connect t
  val connect_to_string : connect t -> string
end

type eid

(** {1 Exceptions} *)

(** {1 Socket management } *)
type error = string * string
val socket : ?domain:domain -> proto -> (socket, error) CCError.t
val socket_exn : ?domain:domain -> proto -> socket
val bind : socket -> Addr.bind Addr.t -> (eid, error) CCError.t
val bind_exn : socket -> Addr.bind Addr.t -> eid
val connect : socket -> Addr.connect Addr.t -> (eid, error) CCError.t
val connect_exn : socket -> Addr.connect Addr.t -> eid
val shutdown : socket -> eid -> (unit, error) CCError.t
val shutdown_exn : socket -> eid -> unit
val close : socket -> (unit, error) CCError.t
val close_exn : socket -> unit
val device : socket -> socket -> (unit, error) CCError.t

(** {1 I/O } *)

(** {2 Zero-copy I/O} *)

val send_bigstring : ?block:bool -> socket -> CCBigstring.t -> (unit, error) CCError.t
val send_bigstring_buf : ?block:bool -> socket -> CCBigstring.t -> int -> int -> (unit, error) CCError.t
val send_string : ?block:bool -> socket -> string -> (unit, error) CCError.t
val send_string_buf : ?block:bool -> socket -> string -> int -> int -> (unit, error) CCError.t
val send_bytes : ?block:bool -> socket -> Bytes.t -> (unit, error) CCError.t
val send_bytes_buf : ?block:bool -> socket -> Bytes.t -> int -> int -> (unit, error) CCError.t

val recv : ?block:bool -> socket -> (CCBigstring.t -> 'a) -> ('a, error) CCError.t
(** [recv ?block sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : ?block:bool -> socket -> (string, error) CCError.t
val recv_bytes : ?block:bool -> socket -> (Bytes.t, error) CCError.t
val recv_bytes_buf :?block:bool -> socket -> Bytes.t -> int -> (int, error) CCError.t

(** {1 Get socket options} *)

val domain : socket -> (domain, error) CCError.t
val proto : socket -> (proto, error) CCError.t
val send_fd : socket -> (Unix.file_descr, error) CCError.t
val recv_fd : socket -> (Unix.file_descr, error) CCError.t

val get_linger : socket -> ([`Inf | `Ms of int], error) CCError.t
val get_send_bufsize : socket -> (int, error) CCError.t
val get_recv_bufsize : socket -> (int, error) CCError.t
val get_send_timeout : socket -> ([`Inf | `Ms of int], error) CCError.t
val get_recv_timeout : socket -> ([`Inf | `Ms of int], error) CCError.t
val get_reconnect_ival : socket -> (int, error) CCError.t
val get_reconnect_ival_max : socket -> (int, error) CCError.t
val get_send_prio : socket -> (int, error) CCError.t
val get_recv_prio : socket -> (int, error) CCError.t
val get_ipv4only : socket -> (bool, error) CCError.t

(** {1 Set socket options} *)

(** {2 General} *)

val set_linger : socket -> [`Inf | `Ms of int] -> (unit, error) CCError.t
val set_send_bufsize : socket -> int -> (unit, error) CCError.t
val set_recv_bufsize : socket -> int -> (unit, error) CCError.t
val set_send_timeout : socket -> [`Inf | `Ms of int] -> (unit, error) CCError.t
val set_recv_timeout : socket -> [`Inf | `Ms of int] -> (unit, error) CCError.t
val set_reconnect_ival : socket -> int -> (unit, error) CCError.t
val set_reconnect_ival_max : socket -> int -> (unit, error) CCError.t
val set_send_prio : socket -> int -> (unit, error) CCError.t
val set_recv_prio : socket -> int -> (unit, error) CCError.t
val set_ipv4_only : socket -> bool -> (unit, error) CCError.t

(** {2 PubSub} *)

val subscribe : socket -> string -> (unit, error) CCError.t
val unsubscribe : socket -> string -> (unit, error) CCError.t

(** {1 Termination} *)

val term : unit -> unit
