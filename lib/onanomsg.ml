open Nanomsg

type error =
  | E_NOT_SUP [@value 156384713]
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
  | E_TERM [@value 156384765]
  | E_FSM
  | E_UNKNOWN (* NOT nanomsg error *) [@@deriving enum]

exception Error of error * string

type domain = AF_SP [@value 1] | AF_SP_RAW [@@deriving enum]

type proto =
  | Pair [@value 16]
  | Pub [@value 32]
  | Sub [@value 33]
  | Req [@value 48]
  | Rep [@value 49]
  | Push [@value 80]
  | Pull [@value 81]
  | Surveyor [@value 96]
  | Respondant [@value 97]
  | Bus [@value 112]
      [@@deriving enum]

type socket = int

let current_error () =
  let current_error_code = nn_errno () in
  (current_error_code, nn_strerror current_error_code)

let throw_current_error () =
  let (code, err_string) = current_error () in
  raise (Error ((match error_of_enum code with
      | Some e -> e
      | None -> E_UNKNOWN), err_string))

let raise_if ~cond v = if cond v then throw_current_error ()
let raise_negative = raise_if ~cond:(fun x -> x < 0)
let raise_not_zero = raise_if ~cond:(fun x -> x <> 0)

let close socket =
  let ret = nn_close socket in
  raise_not_zero ret

module Ipaddr = struct
  include Ipaddr
  let pp = pp_hum
end

type addr = [`Inproc of string | `Ipc of string | `Tcp of Ipaddr.t * int]

let string_of_addr = function
  | `Inproc a -> "inproc://" ^ a
  | `Ipc a -> "ipc://" ^ a
  | `Tcp (a, p) -> "tcp://" ^ Ipaddr.to_string a ^ ":" ^ string_of_int p

let addr_of_string s =
  let len = String.length s in
  let addr_start = String.index s '/' + 2 in
  let addr_len = len - addr_start in
  match String.sub s 0 (addr_start - 3) with
  | "inproc" -> `Inproc (String.sub s addr_start addr_len)
  | "ipc" -> `Ipc (String.sub s addr_start addr_len)
  | "tcp" ->
    let port_start = String.rindex s ':' + 1 in
    let port = String.sub s port_start (len - port_start) in
    let port, port_len = int_of_string port, String.length port in
    let addr = Ipaddr.of_string_exn @@
      String.sub s addr_start (addr_len - port_len - 1) in
    `Tcp (addr, port)
  | _ -> invalid_arg "addr_of_string"

let socket ~domain ~proto = nn_socket (domain_to_enum domain) (proto_to_enum proto)

type eid = int

let bind socket addr =
  let addr = string_of_addr addr in
  let endpoint = nn_bind socket addr in
  raise_negative endpoint;
  endpoint

let connect socket addr =
  let addr = string_of_addr addr in
  let endpoint = nn_connect socket addr in
  raise_negative endpoint;
  endpoint

let shutdown s e = raise_negative (nn_shutdown s e)

let send ?(block=true) socket msg =
  let flag = if block then 0 else nn_dontwait in
  let unsigned_length = Unsigned.Size_t.of_int (String.length msg) in
  let read = nn_send socket msg unsigned_length flag in
  raise_negative read

let recv ?(block=true) socket =
  let open Ctypes in
  let flag = if block then 0 else nn_dontwait in
  let s = allocate string_opt None in
  let read = nn_recv socket s nn_msg flag in
  raise_negative read;
  match !@ s with
  | None -> assert false
  | Some x -> x

let subscribe socket topic =
  let open Ctypes in
  let opt_length = Unsigned.Size_t.of_int (String.length topic) in
  let topic_ptr = to_voidp (allocate string topic) in
  let v = nn_setsockopt socket (proto_to_enum Sub) 1 topic_ptr opt_length in
  raise_negative v

let unsubscribe socket topic =
  let open Ctypes in
  let opt_length = Unsigned.Size_t.of_int (String.length topic) in
  let topic_ptr = to_voidp (allocate string topic) in
  let v = nn_setsockopt socket (proto_to_enum Sub) 2 topic_ptr opt_length in
  raise_negative v

(* helper function to set options *)
let set_option s typ ~option ~value =
  let open Ctypes in
  let opt_length = Unsigned.Size_t.of_int (sizeof typ) in
  let option_ptr = to_voidp (allocate typ value) in
  raise_negative (nn_setsockopt s nn_sol_socket option option_ptr opt_length)

let inf_to_val = function
  | `Infinite -> -1
  | `Milliseconds x -> x

let set_linger socket v =
  set_option socket Ctypes.int ~option:nn_linger ~value:(inf_to_val v)

let set_send_buffer socket size =
  set_option socket Ctypes.int ~option:nn_sndbuf ~value:size

let set_recv_buffer socket size =
  set_option socket Ctypes.int ~option:nn_rcvbuf ~value:size

let set_send_timeout socket v =
  set_option socket Ctypes.int ~option:nn_sndtimeo ~value:(inf_to_val v)

let set_recv_timeout socket v =
  set_option socket Ctypes.int ~option:nn_rcvtimeo ~value:(inf_to_val v)

let set_reconnect_interval socket milliseconds =
  set_option socket Ctypes.int ~option:nn_reconnect_ivl ~value:milliseconds

let set_send_priority socket priority =
  if priority >= 1 || priority <= 16 then
    set_option socket Ctypes.int ~option:nn_sndprio ~value:priority
  else
    invalid_arg (
      Printf.sprintf "set_send_priority: priority(%d) must be between [1,16]" 
      priority
    )

let set_ipv4_only socket v =
  let value = if v then 1 else 0 in
  set_option socket Ctypes.int ~option:nn_ipv4only ~value

let term = nn_term
