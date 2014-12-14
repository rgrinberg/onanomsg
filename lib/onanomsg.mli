type error =
  | E_NOT_SUP
  | E_PROTO_NO_SUPPORT
  | E_NO_BUFFS
  | E_NET_DOWN
  | E_ADDR_IN_USE
  | E_ADDR_NOT_AVAIL
  | E_CONN_REFUSED
  | E_IN_PROGRESS
  | E_NOT_SOCK
  | E_AF_NO_SUPPORT
  | E_PROTO
  | E_AGAIN
  | E_BAD_F
  | E_INVAL
  | E_MFILE
  | E_FAULT
  | E_ACCCESS
  | E_NET_RESET
  | E_NET_UNREACH
  | E_HOST_UNREACH
  | E_NOT_CONN
  | E_MSG_SIZE
  | E_TIMED_OUT
  | E_CONN_ABORTED
  | E_CONN_RESET
  | E_NO_PROTO_OPT
  | E_IS_CONN
  | E_TERM
  | E_FSM
  | E_UNKNOWN

exception Error of error * string

type domain = AF_SP | AF_SP_RAW
type proto = Pair | Pub | Sub | Req | Rep | Push | Pull | Surveyor | Respondant | Bus
type addr = [`Inproc of string | `Ipc of string | `Tcp of Ipaddr.t * int]
type socket
type eid

val string_of_addr : addr -> string
val addr_of_string : string -> addr

val socket : domain:domain -> proto:proto -> socket

val bind : socket -> addr -> eid
val connect : socket -> addr -> eid

val send : ?block:bool -> socket -> string -> unit
val recv : ?block:bool -> socket -> string

val shutdown : socket -> eid -> unit
val close : socket -> unit

(** {1 Publish-Subscribe} *)

val subscribe : socket -> string -> unit
val unsubscribe : socket -> string -> unit

(** {1 Set socket options} *)

val set_linger : socket -> [< `Infinite | `Milliseconds of int] -> unit
val set_send_buffer : socket -> int -> unit
val set_recv_buffer : socket -> int -> unit
val set_send_timeout : socket -> [< `Infinite | `Milliseconds of int] -> unit
val set_recv_timeout : socket -> [< `Infinite | `Milliseconds of int] -> unit
val set_reconnect_interval : socket -> int -> unit
val set_send_priority : socket -> int -> unit
val set_ipv4_only : socket -> bool -> unit

(** {1 Misc.} *)

val term : unit -> unit
