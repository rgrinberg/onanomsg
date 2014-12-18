open Nanomsg_ctypes
open Nanomsg_utils

exception Error = Nanomsg_utils.Error

type socket = int
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

type eid = int

let socket ?(domain=AF_SP) proto =
  raise_negative (fun () ->
      nn_socket (domain_to_enum domain) (proto_to_enum proto))

let bind sock addr =
  raise_negative (fun () -> nn_bind sock @@ string_of_addr addr)

let connect sock addr =
  raise_negative (fun () -> nn_connect sock @@ string_of_addr addr)

let shutdown s e =
  ignore @@ raise_negative (fun () -> nn_shutdown s e)

let close sock =
  ignore @@ raise_notequal 0 (fun () -> nn_close sock)

(* getsockopt *)

let getsockopt ~typ ~init sock level opt =
  let open Ctypes in
  let p = allocate typ init in
  let size = allocate size_t @@ size_of_int (sizeof typ) in
  ignore @@ raise_negative (fun () ->
      nn_getsockopt sock
        Symbol.(value_of_name_exn level)
        Symbol.(value_of_name_exn opt)
        (to_voidp p) size
    ); !@ p

let getsockopt_int = getsockopt ~typ:Ctypes.int ~init:0

let domain sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_DOMAIN" |>
  domain_of_enum |> Opt.run

let proto sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_PROTOCOL" |>
  proto_of_enum |> Opt.run

let get_linger sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_LINGER" |> function
  | n when n < 0 -> `Inf
  | n -> `Ms n

let get_send_bufsize sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDBUF"

let get_recv_bufsize sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVBUF"

let get_send_timeout sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDTIMEO" |> function
  | n when n < 0 -> `Inf
  | n -> `Ms n

let get_recv_timeout sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVTIMEO" |> function
  | n when n < 0 -> `Inf
  | n -> `Ms n

let get_reconnect_ival sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RECONNECT_IVL"

let get_reconnect_ival_max sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RECONNECT_IVL_MAX"

let get_send_prio sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDPRIO"

let get_recv_prio sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVPRIO"

let get_ipv4only sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_IPV4ONLY" |> bool_of_int

let send_fd sock =
  let fd = getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDFD" in
  (Obj.magic fd : Unix.file_descr)

let recv_fd sock =
  let fd = getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVFD" in
  (Obj.magic fd : Unix.file_descr)

let send ?(block=true) sock buf =
  let open Cstruct in
  let nn_buf = nn_allocmsg (size_of_int buf.len) 0 in
  match nn_buf with
  | None -> throw ()
  | Some nn_buf ->
    let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
    let ba = of_bigarray @@
      Ctypes.(bigarray_of_ptr array1 buf.len
                Bigarray.char @@ from_voidp char nn_buf) in
    blit buf buf.off ba 0 buf.len;
    ignore @@ raise_notequal buf.len
      (fun () -> nn_send sock nn_buf_p nn_msg (int_of_bool block))

let send_from_bytes ?(block=true) sock buf pos len =
  if pos < 0 || len < 0 || pos + len > Bytes.length buf
  then invalid_arg "bounds";
  let nn_buf = nn_allocmsg (size_of_int len) 0 in
  match nn_buf with
  | None -> throw ()
  | Some nn_buf ->
    let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
    let ba = Cstruct.of_bigarray @@ Ctypes.(bigarray_of_ptr array1 len
                       Bigarray.char @@ from_voidp char nn_buf) in
    Cstruct.blit_from_bytes buf pos ba 0 len;
    ignore @@ raise_notequal len
      (fun () -> nn_send sock nn_buf_p nn_msg (int_of_bool block))

let send_from_string ?(block=true) sock s =
  send_from_bytes ~block sock (Bytes.unsafe_of_string s) 0 (String.length s)

let recv ?(block=true) sock f =
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  let nb_recv =
    raise_negative
      (fun () -> nn_recv sock ba_start_p nn_msg (int_of_bool block)) in
  let ba_start = !@ ba_start_p in
  if nb_recv < 0 then throw ()
  else
    let ba = Cstruct.of_bigarray @@ bigarray_of_ptr array1 nb_recv
        Bigarray.char (from_voidp char ba_start) in
    let res = f ba in
    let (_:int) = nn_freemsg ba_start in
    res

let recv_to_string ?(block=true) sock f =
  recv ~block sock (fun ba ->
      let buf = Bytes.create ba.Cstruct.len in
      Cstruct.(blit_to_bytes ba 0 buf 0 ba.len);
      f @@ Bytes.unsafe_to_string buf
    )

let setsockopt sock level opt optval optvalsize =
  let open Ctypes in
  ignore @@ raise_negative (fun () ->
      nn_setsockopt sock
        (Symbol.value_of_name_exn level)
        (Symbol.value_of_name_exn opt)
        (to_voidp optval)
        (size_of_int optvalsize)
    )

let setsockopt_int sock level opt v =
  let open Ctypes in
  setsockopt sock level opt (allocate int v) (sizeof int)


let subscribe sock topic =
  setsockopt sock "NN_SUB" "NN_SUB_SUBSCRIBE"
    Ctypes.(allocate string topic) (String.length topic)

let unsubscribe sock topic =
  setsockopt sock "NN_SUB" "NN_SUB_UNSUBSCRIBE"
    Ctypes.(allocate string topic) (String.length topic)


let set_linger sock duration =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_LINGER" (int_of_duration duration)

let set_send_bufsize sock size =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_SNDBUF" size

let set_recv_bufsize sock size =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_RCVBUF" size

let set_send_timeout sock duration =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_SNDTIMEO" (int_of_duration duration)

let set_recv_timeout sock duration =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_RCVTIMEO" (int_of_duration duration)

let set_reconnect_ival sock ival =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_RECONNECT_IVL" ival

let set_reconnect_ival_max sock ival =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_RECONNECT_IVL_MAX" ival

let set_send_prio sock priority =
  if priority < 1 || priority > 16 then invalid_arg "set_send_priority";
  setsockopt_int sock "NN_SOL_SOCKET" "NN_SNDPRIO" priority

let set_recv_prio sock priority =
  if priority < 1 || priority > 16 then invalid_arg "set_recv_priority";
  setsockopt_int sock "NN_SOL_SOCKET" "NN_RCVPRIO" priority

let set_ipv4_only sock b =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_IPV4ONLY" (int_of_bool b)

let term = nn_term
