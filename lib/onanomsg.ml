open Nanomsg

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
  | E_UNKNOWN (* NOT nanomsg error *)

let error_of_int i = match i with
  | _ when i = enotsup         -> E_NOT_SUP
  | _ when i = eprotonosupport -> E_PROTO_NO_SUPPORT
  | _ when i = enobufs         -> E_NO_BUFFS
  | _ when i = enetdown        -> E_NET_DOWN
  | _ when i = eaddrinuse      -> E_ADDR_IN_USE
  | _ when i = eaddrnotavail   -> E_ADDR_NOT_AVAIL
  | _ when i = econnrefused    -> E_CONN_REFUSED
  | _ when i = einprogress     -> E_IN_PROGRESS
  | _ when i = enotsock        -> E_NOT_SOCK
  | _ when i = eafnosupport    -> E_AF_NO_SUPPORT
  | _ when i = eproto          -> E_PROTO
  | _ when i = eagain          -> E_AGAIN
  | _ when i = ebadf           -> E_BAD_F
  | _ when i = einval          -> E_INVAL
  | _ when i = emfile          -> E_MFILE
  | _ when i = efault          -> E_FAULT
  | _ when i = eaccess         -> E_ACCCESS
  | _ when i = enetreset       -> E_NET_RESET
  | _ when i = enetunreach     -> E_NET_UNREACH
  | _ when i = ehostunreach    -> E_HOST_UNREACH
  | _ when i = enotconn        -> E_NOT_CONN
  | _ when i = emsgsize        -> E_MSG_SIZE
  | _ when i = etimedout       -> E_TIMED_OUT
  | _ when i = econnaborted    -> E_CONN_ABORTED
  | _ when i = econnreset      -> E_CONN_RESET
  | _ when i = enoprotoopt     -> E_NO_PROTO_OPT
  | _ when i = eisconn         -> E_IS_CONN
  | _ when i = eterm           -> E_TERM
  | _ when i = efsm            -> E_FSM
  | _ -> E_UNKNOWN

exception Error of error * string

module Domain = struct
  type t =
    | Af_sp
    | Af_sp_raw

  let to_int = function
    | Af_sp -> af_sp
    | Af_sp_raw -> af_sp_raw
end

type sock_type = [
  | `Pair (* pair *)
  | `Pub (* pub sub *)
  | `Sub
  | `Req (* req rep *)
  | `Rep
  | `Push (* pipeline *)
  | `Pull
  | `Surveyor (* survey *)
  | `Respondent
  | `Bus ] (* bus *)

let current_error () = 
  let current_error_code = nn_errno () in
  (current_error_code, nn_strerror current_error_code)

let throw_current_error () = 
  let (code, err_string) = current_error () in
  raise (Error (error_of_int code, err_string))

let raise_if ~cond v = if cond v then throw_current_error ()
let raise_negative = raise_if ~cond:(fun x -> x < 0)
let raise_not_zero = raise_if ~cond:(fun x -> x <> 0)

type endpoint = Endpoint of int

type fd = int

module Socket = struct
  type 'a t = Socket of int
  type 'a kind = int

  let pair       = Pair.nn_pair
  let pub        = Pub_sub.nn_pub
  let sub        = Pub_sub.nn_sub
  let req        = Req_rep.nn_req
  let rep        = Req_rep.nn_rep
  let push       = Pipeline.nn_push
  let pull       = Pipeline.nn_pull
  let surveyor   = Survey.nn_surveyor
  let respondent = Survey.nn_respondent
  let bus        = Bus.nn_bus

  let socket ~domain ~sock_type =
    let ret = nn_socket (Domain.to_int domain) sock_type in
    raise_negative ret;
    Socket ret
end

let int_of_sock_type = function
  | `Pair -> Pair.nn_pair
  | `Pub -> Pub_sub.nn_pub
  | `Sub -> Pub_sub.nn_sub
  | `Req -> Req_rep.nn_req
  | `Rep -> Req_rep.nn_rep
  | `Push -> Pipeline.nn_push
  | `Pull -> Pipeline.nn_pull
  | `Surveyor -> Survey.nn_surveyor
  | `Respondent -> Survey.nn_respondent
  | `Bus -> Bus.nn_bus

open Socket

let fd (Socket fd) = fd

let close (Socket socket) =
  let ret = nn_close socket in
  raise_not_zero ret

let bind (Socket socket) ~address =
  let endpoint = nn_bind socket address in
  raise_negative endpoint;
  Endpoint endpoint

let connect (Socket socket) ~address =
  let endpoint = nn_connect socket address in
  raise_negative endpoint;
  Endpoint endpoint

let send ?(block=true) (Socket socket) str =
  let flag = if block then 0 else nn_dontwait in
  let unsigned_length = Unsigned.Size_t.of_int (String.length str) in
  let read = nn_send socket str unsigned_length flag in
  raise_negative read

let recv ?(block=true) (Socket socket) =
  let open Ctypes in
  let flag = if block then 0 else nn_dontwait in
  let s = allocate string_opt None in
  let read = nn_recv socket s nn_msg flag in
  raise_negative read;
  match !@ s with
  | None -> failwith "TEMP STUFF"
  | Some x -> x

let subscribe (Socket socket) ~topic =
  let open Ctypes in
  let opt_length = Unsigned.Size_t.of_int (String.length topic) in
  let topic_ptr = to_voidp (allocate string topic) in
  let v = nn_setsockopt socket 
    Pub_sub.nn_sub Pub_sub.nn_sub_subscribe
    topic_ptr opt_length in
  raise_negative v

let unsubscribe (Socket socket) ~topic =
  let open Ctypes in
  let opt_length = Unsigned.Size_t.of_int (String.length topic) in
  let topic_ptr = to_voidp (allocate string topic) in
  let v = nn_setsockopt socket 
    Pub_sub.nn_sub Pub_sub.nn_sub_unsubscribe
    topic_ptr opt_length in
  raise_negative v

(* helper function to set options *)
let set_option (Socket s) typ ~option ~value =
  let open Ctypes in
  let opt_length = Unsigned.Size_t.of_int (sizeof typ) in
  let option_ptr = to_voidp (allocate typ value) in
  raise_negative (nn_setsockopt s nn_sol_socket option option_ptr opt_length)

let set_linger socket = function
  | `Infinite -> set_option socket Ctypes.int ~option:nn_linger ~value:(-1)
  | `Milliseconds value -> 
      set_option socket Ctypes.int ~option:nn_linger ~value
