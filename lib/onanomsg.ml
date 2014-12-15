open Nanomsg

module Symbol = struct
  type t = {
    sp_value: int;
    sp_name: string;
    sp_ns: int;
    sp_type: int;
    sp_unit: int;
  }

  let table = Hashtbl.create 13

  (* This will be run at program start. *)
  let () =
    let open Ctypes in
    let rec inner i =
      let sp = make nn_symbol_properties in
      let ret = nn_symbol_info i (addr sp) (sizeof nn_symbol_properties) in
      if ret = 0 then () else
        let sp' =
          {
            sp_value = getf sp nnsym_value;
            sp_name = getf sp nnsym_name;
            sp_ns = getf sp nnsym_ns;
            sp_type = getf sp nnsym_type;
            sp_unit = getf sp nnsym_unit;
          } in
        Hashtbl.add table sp'.sp_name sp';
        inner @@ succ i
    in inner 0

  let value_of_name_exn name = let sp = Hashtbl.find table name in sp.sp_value
  let value_of_name name = try Some (value_of_name_exn name) with _ -> None
  let of_name_exn = Hashtbl.find table
  let of_name name = try Some (of_name_exn name) with _ -> None
end

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
  | E_SOCKT_NO_SUPPORT
  | E_TERM [@value 156384765]
  | E_FSM
  | E_UNKNOWN (* NOT nanomsg error *) [@@deriving enum,show]

exception Error of error * string * string

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
  let e = match error_of_enum code with Some e -> e | None -> E_UNKNOWN in
  raise (Error (e, show_error e, err_string))

let raise_if cond f =
  let res = f () in
  if cond res then throw_current_error () else res

let raise_negative = raise_if (fun x -> x < 0)
let raise_not_zero = raise_if (fun x -> x <> 0)


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
  raise_negative (fun () -> nn_bind socket @@ string_of_addr addr)

let connect socket addr =
  raise_negative (fun () -> nn_connect socket @@ string_of_addr addr)

let shutdown s e =
  ignore @@ raise_negative (fun () -> nn_shutdown s e)

let close socket =
  ignore @@ raise_not_zero (fun () -> nn_close socket)

let size_of_int = Unsigned.Size_t.of_int

module B = struct
  let send socket buf pos len =
    if pos < 0 || len < 0 || pos + len > Lwt_bytes.length buf
    then invalid_arg "bounds";
    let nn_buf = nn_allocmsg (size_of_int len) 0 in
    match nn_buf with
    | None -> throw_current_error ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      Lwt_bytes.blit buf pos ba 0 len;
      let nb_sent = raise_negative
          (fun () -> nn_send socket nn_buf_p nn_msg 0) in
      assert (nb_sent = len)

  let send_from_bytes socket buf pos len =
    if pos < 0 || len < 0 || pos + len > Bytes.length buf
    then invalid_arg "bounds";
    let nn_buf = nn_allocmsg (size_of_int len) 0 in
    match nn_buf with
    | None -> throw_current_error ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      Lwt_bytes.blit_from_bytes buf pos ba 0 len;
      let nb_sent = raise_negative
          (fun () -> nn_send socket nn_buf_p nn_msg 0) in
      assert (nb_sent = len)

  let send_from_string socket s =
    send_from_bytes socket (Bytes.unsafe_of_string s) 0 (String.length s)

  let recv socket f =
    let open Ctypes in
    let ba_start_p = allocate (ptr void) null in
    let nb_recv = nn_recv socket ba_start_p nn_msg 0 in
    let ba_start = !@ ba_start_p in
    if nb_recv < 0 then throw_current_error ()
    else
      let ba = bigarray_of_ptr array1 nb_recv
          Bigarray.char (from_voidp char ba_start) in
      let res = f ba nb_recv in
      let (_:int) = nn_freemsg ba_start in
      res

  let recv_to_string socket f =
    recv socket (fun ba len ->
        let buf = Bytes.create len in
        Lwt_bytes.blit_to_bytes ba 0 buf 0 len;
        f @@ Bytes.unsafe_to_string buf
      )
end

let subscribe socket topic =
  let open Ctypes in
  ignore @@ raise_negative (fun () ->
      nn_setsockopt socket
        Symbol.(value_of_name_exn "NN_SUB")
        Symbol.(value_of_name_exn "NN_SUB_SUBSCRIBE")
        (to_voidp (allocate string topic))
        (size_of_int (String.length topic))
    )

let unsubscribe socket topic =
  let open Ctypes in
  ignore @@ raise_negative (fun () ->
      nn_setsockopt socket
        Symbol.(value_of_name_exn "NN_SUB")
        Symbol.(value_of_name_exn "NN_SUB_UNSUBSCRIBE")
        (to_voidp (allocate string topic))
        (size_of_int (String.length topic))
    )

(* helper function to set options *)
let set_option s typ ~option ~value =
  let open Ctypes in
  ignore @@ raise_negative (fun () ->
      nn_setsockopt s
        (Symbol.value_of_name_exn "NN_SOL_SOCKET")
        option
        (to_voidp (allocate typ value))
        (size_of_int (sizeof typ))
    )

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

let send_fd socket =
  let open Ctypes in
  let fd_p = allocate int 0 in
  let size = allocate size_t @@ size_of_int (sizeof int) in
  ignore @@ raise_negative (fun () ->
      nn_getsockopt socket
        Symbol.(value_of_name_exn "NN_SOL_SOCKET")
        Symbol.(value_of_name_exn "NN_SENDFD")
        (to_voidp fd_p) size
    );
  (Obj.magic (!@ fd_p) : Unix.file_descr)

let recv_fd socket =
  let open Ctypes in
  let fd_p = allocate int 0 in
  let size = allocate size_t @@ size_of_int (sizeof int) in
  ignore @@ raise_negative (fun () ->
      nn_getsockopt socket
        Symbol.(value_of_name_exn "NN_SOL_SOCKET")
        Symbol.(value_of_name_exn "NN_RECVFD")
        (to_voidp fd_p) size
    );
  (Obj.magic (!@ fd_p) : Unix.file_descr)

let term = nn_term
