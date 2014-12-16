module Symbol : sig
  type t = private {
    sp_value: int;
    sp_name: string;
    sp_ns: int;
    sp_type: int;
    sp_unit: int;
  }

  val value_of_name : string -> int option
  val of_name : string -> t option
  val value_of_name_exn : string -> int
  val of_name_exn : string -> t
  val errvalue_of_errno_exn : int -> string
  val errvalue_of_errno : int -> string option
end

exception Error of string * string

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

module B : sig
  val send : socket -> Lwt_bytes.t -> int -> int -> unit
  val send_from_bytes : socket -> Bytes.t -> int -> int -> unit
  val send_from_string : socket -> string -> unit

  val recv : socket -> (Lwt_bytes.t -> int -> 'a) -> 'a
  val recv_to_string : socket -> (string -> 'a) -> 'a
end

module NB : sig
  val send : socket -> Lwt_bytes.t -> int -> int -> unit Lwt.t
  val send_from_bytes : socket -> Bytes.t -> int -> int -> unit Lwt.t
  val send_from_string : socket -> string -> unit Lwt.t

  val recv : socket -> (Lwt_bytes.t -> int -> 'a Lwt.t) -> 'a Lwt.t
  val recv_to_string : socket -> (string -> 'a Lwt.t) -> 'a Lwt.t
end

val shutdown : socket -> eid -> unit
val close : socket -> unit

(** {1 Publish-Subscribe} *)

val subscribe : socket -> string -> unit
val unsubscribe : socket -> string -> unit

(** {1 Get socket options} *)

val domain : socket -> domain
val proto : socket -> proto
val get_linger : socket -> [`Inf | `Ms of int]
val get_send_bufsize : socket -> int
val get_recv_bufsize : socket -> int
val get_send_timeout : socket -> [`Inf | `Ms of int]
val get_recv_timeout : socket -> [`Inf | `Ms of int]

val send_fd : socket -> Unix.file_descr
val recv_fd : socket -> Unix.file_descr

(** {1 Set socket options} *)

val set_linger : socket -> [`Inf | `Ms of int] -> unit
val set_send_bufsize : socket -> int -> unit
val set_recv_bufsize : socket -> int -> unit
val set_send_timeout : socket -> [`Inf | `Ms of int] -> unit
val set_recv_timeout : socket -> [`Inf | `Ms of int] -> unit
val set_reconnect_interval : socket -> int -> unit
val set_send_priority : socket -> int -> unit
val set_ipv4_only : socket -> bool -> unit

(** {1 Misc.} *)

val term : unit -> unit
