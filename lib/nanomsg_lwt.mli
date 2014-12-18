open Nanomsg

val send : socket -> Lwt_bytes.t -> int -> int -> unit Lwt.t
val send_from_bytes : socket -> Bytes.t -> int -> int -> unit Lwt.t
val send_from_string_raw : socket -> string -> int -> int -> unit Lwt.t
val send_from_string : socket -> string -> unit Lwt.t

val recv : socket -> (Lwt_bytes.t -> 'a Lwt.t) -> 'a Lwt.t
val recv_to_string : socket -> (string -> 'a Lwt.t) -> 'a Lwt.t
