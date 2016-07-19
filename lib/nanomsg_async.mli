open Core.Std
open Async.Std
open Nanomsg

(** {1 Asynchronous I/O} *)

(** {2 Zero-copy I/O} *)

val send_bigstring : socket -> Bigstring.t ->
  (unit, error) Result.result Deferred.t

val send_bigstring_buf : socket -> Bigstring.t -> int -> int ->
  (unit, error) Result.result  Deferred.t

val send_string : socket -> string -> (unit, error) Result.result  Deferred.t

val send_string_buf : socket -> string -> int -> int ->
  (unit, error) Result.result Deferred.t

val send_bytes : socket -> Bytes.t -> (unit, error) Result.result Deferred.t

val send_bytes_buf : socket -> Bytes.t -> int -> int ->
  (unit, error) Result.result Deferred.t

val recv : socket -> (Bigstring.t -> 'a Deferred.t) -> ('a, error) Result.result Deferred.t
(** [recv sock f] applies [f] to the received message. The
    argument of [f] gets unallocated after [f] returns, so make sure
    [f] {b never} let a reference to its argument escape. *)

(** {2 Legacy I/O} *)

val recv_string : socket -> (string, error) Result.result Deferred.t
val recv_bytes : socket -> (Bytes.t, error) Result.result  Deferred.t
val recv_bytes_buf : socket -> Bytes.t -> int -> (int, error) Result.result Deferred.t
