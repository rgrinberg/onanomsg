open Nanomsg_utils

type error = string * string
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
  | Surveyor [@value 98]
  | Respondant [@value 99]
  | Bus [@value 112]
      [@@deriving enum]

module Addr = struct
  module V4 = struct
    include Ipaddr.V4
    let pp = pp_hum
  end

  module V6 = struct
    include Ipaddr.V6
    let pp = pp_hum
  end

  type bind = [
    | `All
    | `V4 of V4.t
    | `V6 of V6.t
    | `Iface of string ]
      [@@deriving show]

  type connect =
    ([`V4 of V4.t | `V6 of V6.t | `Dns of string] *
     [`V4 of V4.t | `V6 of V6.t | `Iface of string] option)
      [@@deriving show]

  type 'a t = [
    | `Inproc of string
    | `Ipc of string
    | `Tcp of 'a * int
  ] [@@deriving show]

  let bind_iface_of_string = function
    | "*" -> `All
    | s when String.contains s ':' -> `V6 (Ipaddr.V6.of_string_exn s)
    | s -> try `V4 (Ipaddr.V4.of_string_exn s) with _ -> `Iface s

  let connect_iface_of_string = function
    | s when String.contains s ':' -> `V6 (Ipaddr.V6.of_string_exn s)
    | s -> try `V4 (Ipaddr.V4.of_string_exn s) with _ -> `Iface s

  let iface_to_string = function
    | `All -> "*"
    | `V4 v4 -> Ipaddr.V4.to_string v4
    | `V6 v6 -> Ipaddr.V6.to_string v6
    | `Iface ifname -> ifname

  let addr_of_string = function
    | s when String.contains s ':' -> `V6 (Ipaddr.V6.of_string_exn s)
    | s -> try `V4 (Ipaddr.V4.of_string_exn s) with _ -> `Dns s

  let addr_to_string = function
    | `V4 v4 -> Ipaddr.V4.to_string v4
    | `V6 v6 -> Ipaddr.V6.to_string v6
    | `Dns n -> n

  let bind_to_string = function
    | `Inproc a -> "inproc://" ^ a
    | `Ipc a -> "ipc://" ^ a
    | `Tcp (bind, port) ->
      let interface = iface_to_string bind in
      "tcp://" ^ interface ^ ":" ^ string_of_int port

  let connect_to_string = function
    | `Inproc a -> "inproc://" ^ a
    | `Ipc a -> "ipc://" ^ a
    | `Tcp ((addr, iface), port) ->
      let iface = CCOpt.map iface_to_string iface in
      let addr = addr_to_string addr in
      "tcp://" ^
      (match iface with Some i -> i ^ ";" | None -> "")
      ^ addr ^ ":" ^ string_of_int port

  let of_string s =
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
      let addr = String.sub s addr_start (addr_len - port_len - 1) in
      `Tcp (addr, port)
    | _ -> invalid_arg "addr_of_string"

  let bind_of_string s = match of_string s with
    | `Inproc _ | `Ipc _ as s -> s
    | `Tcp (addr, port) ->
      let iface = bind_iface_of_string addr in
      `Tcp (iface, port)

  let connect_of_string s = match of_string s with
    | `Inproc _ | `Ipc _ as s -> s
    | `Tcp (iface_addr, port) ->
      if String.contains iface_addr ';' then
        let len = String.length iface_addr in
        let addr_start = String.index iface_addr ';' + 1 in
        let addr = String.sub iface_addr addr_start (len - addr_start) in
        let iface = String.(sub iface_addr 0 @@ addr_start - 1) in
        `Tcp ((addr_of_string addr, Some (connect_iface_of_string iface)), port)
      else
        `Tcp ((addr_of_string iface_addr, None), port)
end


type eid = int

let socket ?(domain=AF_SP) proto =
  error_if_negative (fun () ->
      C.nn_socket (domain_to_enum domain) (proto_to_enum proto))

let socket_exn ?(domain=AF_SP) proto =
  socket ~domain proto |> CCError.get_exn

let bind sock addr =
  error_if_negative (fun () -> C.nn_bind sock @@ Addr.bind_to_string addr)

let bind_exn sock addr =
  bind sock addr |> CCError.get_exn

let connect sock addr =
  error_if_negative (fun () -> C.nn_connect sock @@ Addr.connect_to_string addr)

let connect_exn sock addr = connect sock addr |> CCError.get_exn

let shutdown s e =
  CCError.map ignore @@
  error_if_notequal 0 (fun () -> C.nn_shutdown s e)

let shutdown_exn s e = shutdown s e |> CCError.get_exn

let close sock =
  CCError.map ignore @@
  error_if_notequal 0 (fun () -> C.nn_close sock)

let close_exn sock = close sock |> CCError.get_exn

(* getsockopt *)

let getsockopt ~typ ~init sock level opt =
  let open CCError in
  let open Ctypes in
  let p = allocate typ init in
  let size = allocate size_t @@ Unsigned.Size_t.of_int (sizeof typ) in
  error_if_notequal 0 (fun () ->
      C.nn_getsockopt sock
        Symbol.(value_of_name_exn level)
        Symbol.(value_of_name_exn opt)
        (to_voidp p) size
    ) >|= fun _ -> !@ p

let getsockopt_int = getsockopt ~typ:Ctypes.int ~init:0

let domain sock =
  let open CCError in
  getsockopt_int sock "NN_SOL_SOCKET" "NN_DOMAIN" >>= fun v ->
  match domain_of_enum v with
  | Some v -> `Ok v
  | None -> `Error ("Internal", "domain_of_enum")

let proto sock =
  let open CCError in
  getsockopt_int sock "NN_SOL_SOCKET" "NN_PROTOCOL" >>= fun v ->
  match proto_of_enum v with
  | Some v -> `Ok v
  | None -> `Error ("Internal", "proto_of_enum")

let get_linger sock =
  CCError.map (fun n -> if n < 0 then `Inf else `Ms n) @@
  getsockopt_int sock "NN_SOL_SOCKET" "NN_LINGER"

let get_send_bufsize sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDBUF"

let get_recv_bufsize sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVBUF"

let get_send_timeout sock =
  CCError.map (fun n -> if n < 0 then `Inf else `Ms n) @@
  getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDTIMEO"

let get_recv_timeout sock =
  CCError.map (fun n -> if n < 0 then `Inf else `Ms n) @@
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVTIMEO"

let get_reconnect_ival sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RECONNECT_IVL"

let get_reconnect_ival_max sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RECONNECT_IVL_MAX"

let get_send_prio sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDPRIO"

let get_recv_prio sock =
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVPRIO"

let get_ipv4only sock =
  CCError.map bool_of_int @@
  getsockopt_int sock "NN_SOL_SOCKET" "NN_IPV4ONLY"

let send_fd sock =
  let open CCError in
  getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDFD" >|= fun fd ->
  (Obj.magic fd : Unix.file_descr)

let recv_fd sock =
  let open CCError in
  getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVFD" >|= fun fd ->
  (Obj.magic fd : Unix.file_descr)

let send_bigstring_buf ?(block=true) sock buf pos len =
  if pos < 0 || len < 0 || pos + len > CCBigstring.size buf
  then invalid_arg "bounds";
  let nn_buf = C.nn_allocmsg (Unsigned.Size_t.of_int len) 0 in
  match nn_buf with
  | None -> error ()
  | Some nn_buf ->
    let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
    let ba = Ctypes.(bigarray_of_ptr array1 len
                       Bigarray.char @@ from_voidp char nn_buf) in
    CCBigstring.blit buf pos ba 0 len;
    CCError.map ignore @@
    error_if_notequal len
      (fun () -> C.nn_send sock nn_buf_p
          (Unsigned.Size_t.of_int (-1)) (int_of_bool @@ not block))

let send_bigstring ?(block=true) sock buf =
  send_bigstring_buf ~block sock buf 0 @@ CCBigstring.size buf

let send_bytes_buf ?(block=true) sock buf pos len =
  if pos < 0 || len < 0 || pos + len > Bytes.length buf
  then invalid_arg "bounds";
  let nn_buf = C.nn_allocmsg (Unsigned.Size_t.of_int len) 0 in
  match nn_buf with
  | None -> error ()
  | Some nn_buf ->
    let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
    let ba = Ctypes.(bigarray_of_ptr array1 len
                       Bigarray.char @@ from_voidp char nn_buf) in
    CCBigstring.blit_of_bytes buf pos ba 0 len;
    CCError.map ignore @@
    error_if_notequal len
      (fun () -> C.nn_send sock nn_buf_p
          (Unsigned.Size_t.of_int (-1)) (int_of_bool @@ not block))

let send_bytes ?(block=true) sock b =
  send_bytes_buf ~block sock b 0 @@ Bytes.length b

let send_string_buf ?(block=true) sock s pos len =
  send_bytes_buf ~block sock (Bytes.unsafe_of_string s) pos len

let send_string ?(block=true) sock s =
  send_bytes_buf ~block sock (Bytes.unsafe_of_string s) 0 (String.length s)

let recv ?(block=true) sock f =
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  let nb_recv =
    error_if_negative
      (fun () -> C.nn_recv sock ba_start_p
          (Unsigned.Size_t.of_int (-1)) (int_of_bool @@ not block)) in
  let ba_start = !@ ba_start_p in
  CCError.map
    (fun nb_recv ->
       let ba = bigarray_of_ptr array1 nb_recv
           Bigarray.char (from_voidp char ba_start) in
       let res = f ba in
       let (_:int) = C.nn_freemsg ba_start in
       res) nb_recv

let recv_bytes_buf ?(block=true) sock buf pos =
  recv ~block sock
    (fun ba ->
       let len = CCBigstring.size ba in
       CCBigstring.(blit_to_bytes ba 0 buf pos len);
       len
    )

let recv_bytes ?(block=true) sock =
  recv ~block sock (fun ba ->
      let len = CCBigstring.size ba in
      let buf = Bytes.create len in
      CCBigstring.blit_to_bytes ba 0 buf 0 len;
      buf)

let recv_string ?(block=true) sock =
  CCError.map Bytes.unsafe_to_string @@ recv_bytes ~block sock

let setsockopt sock level opt optval optvalsize =
  let open Ctypes in
  error_if_negative_ign (fun () ->
      C.nn_setsockopt sock
        (Symbol.value_of_name_exn level)
        (Symbol.value_of_name_exn opt)
        (to_voidp optval)
        (Unsigned.Size_t.of_int optvalsize)
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

let term = C.nn_term

let device s1 s2 =
  (fun () -> C.nn_device s1 s2)
  |> error_if_negative
  |> CCError.map ignore
