type domain = AF_SP | AF_SP_RAW
type proto = Pair | Pub | Sub | Req | Rep | Push | Pull | Surveyor | Respondant | Bus
type socket

module Addr : sig
  type bind = [
    | `All
    | `V4 of Ipaddr.V4.t
    | `V6 of Ipaddr.V6.t
    | `Iface of string ] * int [@@deriving show]

  type connect =
    ([`V4 of Ipaddr.V4.t | `V6 of Ipaddr.V6.t | `Dns of string] *
     [`V4 of Ipaddr.V4.t | `V6 of Ipaddr.V6.t | `Iface of string] option) * int
      [@@deriving show]

  type 'a t = [
    | `Inproc of string
    | `Ipc of string
    | `Tcp of 'a
  ] [@@deriving show]

  val bind_of_string : string -> bind t
  val bind_to_string : bind t -> string
  val connect_of_string : string -> connect t
  val connect_to_string : connect t -> string
end

type eid

(** {1 Exceptions} *)

exception Error of string * string

(** {1 Socket management } *)

val socket : ?domain:domain -> proto -> socket
val bind : socket -> Addr.bind Addr.t -> eid
val connect : socket -> Addr.connect Addr.t -> eid
val shutdown : socket -> eid -> unit
val close : socket -> unit

(** {1 I/O } *)

(** {2 Zero-copy I/O} *)

val send : ?block:bool -> socket -> Cstruct.t -> unit
val recv : ?block:bool -> socket -> (Cstruct.t -> 'a) -> 'a
(** [recv ?block sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val send_string : ?block:bool -> socket -> string -> unit
val send_bytes : ?block:bool -> socket -> Bytes.t -> unit

val send_string_buf : ?block:bool -> socket -> string -> int -> int -> unit
val send_bytes_buf : ?block:bool -> socket -> Bytes.t -> int -> int -> unit

val recv_string : ?block:bool -> socket -> string
val recv_bytes : ?block:bool -> socket -> Bytes.t

val recv_bytes_buf :?block:bool -> socket -> Bytes.t -> int -> unit

(** {1 Get socket options} *)

val domain : socket -> domain
val proto : socket -> proto
val send_fd : socket -> Unix.file_descr
val recv_fd : socket -> Unix.file_descr

val get_linger : socket -> [`Inf | `Ms of int]
val get_send_bufsize : socket -> int
val get_recv_bufsize : socket -> int
val get_send_timeout : socket -> [`Inf | `Ms of int]
val get_recv_timeout : socket -> [`Inf | `Ms of int]
val get_reconnect_ival : socket -> int
val get_reconnect_ival_max : socket -> int
val get_send_prio : socket -> int
val get_recv_prio : socket -> int
val get_ipv4only : socket -> bool

(** {1 Set socket options} *)

(** {2 General} *)

val set_linger : socket -> [`Inf | `Ms of int] -> unit
val set_send_bufsize : socket -> int -> unit
val set_recv_bufsize : socket -> int -> unit
val set_send_timeout : socket -> [`Inf | `Ms of int] -> unit
val set_recv_timeout : socket -> [`Inf | `Ms of int] -> unit
val set_reconnect_ival : socket -> int -> unit
val set_reconnect_ival_max : socket -> int -> unit
val set_send_prio : socket -> int -> unit
val set_recv_prio : socket -> int -> unit
val set_ipv4_only : socket -> bool -> unit

(** {2 PubSub} *)

val subscribe : socket -> string -> unit
val unsubscribe : socket -> string -> unit

(** {1 Termination} *)

val term : unit -> unit
