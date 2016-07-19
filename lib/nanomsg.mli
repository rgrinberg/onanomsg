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
val socket : ?domain:domain -> proto -> (socket, error) Result.result
val socket_exn : ?domain:domain -> proto -> socket
val bind : socket -> Addr.bind Addr.t -> (eid, error) Result.result
val bind_exn : socket -> Addr.bind Addr.t -> eid
val connect : socket -> Addr.connect Addr.t -> (eid, error) Result.result
val connect_exn : socket -> Addr.connect Addr.t -> eid
val shutdown : socket -> eid -> (unit, error) Result.result
val shutdown_exn : socket -> eid -> unit
val close : socket -> (unit, error) Result.result
val close_exn : socket -> unit
val device : socket -> socket -> (unit, error) Result.result

(** {1 I/O } *)

(** {2 Zero-copy I/O} *)

val send_bigstring : ?block:bool -> socket -> Bigstring.t -> (unit, error) Result.result
val send_bigstring_buf : ?block:bool -> socket -> Bigstring.t -> int -> int -> (unit, error) Result.result
val send_string : ?block:bool -> socket -> string -> (unit, error) Result.result
val send_string_buf : ?block:bool -> socket -> string -> int -> int -> (unit, error) Result.result
val send_bytes : ?block:bool -> socket -> Bytes.t -> (unit, error) Result.result
val send_bytes_buf : ?block:bool -> socket -> Bytes.t -> int -> int -> (unit, error) Result.result

val recv : ?block:bool -> socket -> (Bigstring.t -> 'a) -> ('a, error) Result.result
(** [recv ?block sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : ?block:bool -> socket -> (string, error) Result.result
val recv_bytes : ?block:bool -> socket -> (Bytes.t, error) Result.result
val recv_bytes_buf :?block:bool -> socket -> Bytes.t -> int -> (int, error) Result.result

(** {1 Get socket options} *)

val domain : socket -> (domain, error) Result.result
val proto : socket -> (proto, error) Result.result
val send_fd : socket -> (Unix.file_descr, error) Result.result
val recv_fd : socket -> (Unix.file_descr, error) Result.result

val get_linger : socket -> ([`Inf | `Ms of int], error) Result.result
val get_send_bufsize : socket -> (int, error) Result.result
val get_recv_bufsize : socket -> (int, error) Result.result
val get_send_timeout : socket -> ([`Inf | `Ms of int], error) Result.result
val get_recv_timeout : socket -> ([`Inf | `Ms of int], error) Result.result
val get_reconnect_ival : socket -> (int, error) Result.result
val get_reconnect_ival_max : socket -> (int, error) Result.result
val get_send_prio : socket -> (int, error) Result.result
val get_recv_prio : socket -> (int, error) Result.result
val get_ipv4only : socket -> (bool, error) Result.result

(** {1 Set socket options} *)

(** {2 General} *)

val set_linger : socket -> [`Inf | `Ms of int] -> (unit, error) Result.result
val set_send_bufsize : socket -> int -> (unit, error) Result.result
val set_recv_bufsize : socket -> int -> (unit, error) Result.result
val set_send_timeout : socket -> [`Inf | `Ms of int] -> (unit, error) Result.result
val set_recv_timeout : socket -> [`Inf | `Ms of int] -> (unit, error) Result.result
val set_reconnect_ival : socket -> int -> (unit, error) Result.result
val set_reconnect_ival_max : socket -> int -> (unit, error) Result.result
val set_send_prio : socket -> int -> (unit, error) Result.result
val set_recv_prio : socket -> int -> (unit, error) Result.result
val set_ipv4_only : socket -> bool -> (unit, error) Result.result

(** {2 PubSub} *)

val subscribe : socket -> string -> (unit, error) Result.result
val unsubscribe : socket -> string -> (unit, error) Result.result

(** {1 Termination} *)

val term : unit -> unit
