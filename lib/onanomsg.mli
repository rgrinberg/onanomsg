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

module Domain : sig 
  type t = 
    | Af_sp 
    | Af_sp_raw 
end

type endpoint

module Socket : sig
  type 'a t
  type 'a kind
  val pair       : [> `Pair] kind
  val pub        : [> `Pub] kind
  val sub        : [> `Sub] kind
  val req        : [> `Req] kind
  val rep        : [> `Rep] kind
  val push       : [> `Push] kind
  val pull       : [> `Pull] kind
  val surveyor   : [> `Surveyor] kind
  val respondent : [> `Respondent] kind
  val bus        : [> `Bus] kind
  val socket : domain:Domain.t -> sock_type:('a kind) -> 'a t
end

val close : 'a Socket.t -> unit

val bind : 'a Socket.t -> address:string -> endpoint

val connect : 'a Socket.t -> address:string -> endpoint

val send : ?block:bool -> 'a Socket.t -> string -> unit

val recv : ?block:bool -> 'a Socket.t -> string

val subscribe : [< `Sub] Socket.t -> topic:string -> unit

val unsubscribe : [< `Sub] Socket.t -> topic:string -> unit
