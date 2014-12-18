type domain = AF_SP | AF_SP_RAW
type proto = Pair | Pub | Sub | Req | Rep | Push | Pull | Surveyor | Respondant | Bus
type socket

type addr = [`Inproc of string | `Ipc of string | `Tcp of Ipaddr.t * int]
val addr_of_string : string -> addr
val string_of_addr : addr -> string

type eid

(** {1 Exceptions} *)

exception Error of string * string


(** {1 Socket management } *)

val socket : ?domain:domain -> proto -> socket
val bind : socket -> addr -> eid
val connect : socket -> addr -> eid
val shutdown : socket -> eid -> unit
val close : socket -> unit

(** {1 I/O } *)

val send : ?block:bool -> socket -> Cstruct.t -> unit
val send_from_bytes : ?block:bool -> socket -> Bytes.t -> int -> int -> unit
val send_from_string : ?block:bool -> socket -> string -> unit

val recv : ?block:bool -> socket -> (Cstruct.t -> 'a) -> 'a
val recv_to_string : ?block:bool -> socket -> (string -> 'a) -> 'a

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
