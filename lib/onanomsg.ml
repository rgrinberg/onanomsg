open Nanomsg
open Lwt.Infix

module Opt = struct
  let run = function
    | Some v -> v
    | None -> invalid_arg "Opt.run"
end

module Ipaddr = struct
  include Ipaddr
  let pp = pp_hum
end

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
  let errvalue_of_errno_exn errno =
    try
      Hashtbl.iter (fun k v ->
          if v.sp_value = errno then failwith v.sp_name)
        table; raise Not_found
    with Failure name -> name
  let errvalue_of_errno errno =
    try Some (errvalue_of_errno_exn errno) with Not_found -> None
end

exception Error of string * string

let throw () =
  let code = nn_errno () in
  let err_string = nn_strerror code in
  let err_value =
    if code > 156384712
    then Symbol.errvalue_of_errno_exn code
    else "" in
  raise (Error (err_value, err_string))

let throw_lwt () = Lwt.wrap1 throw ()

let raise_if cond f =
  let res = f () in
  if cond res then throw () else res

let raise_negative = raise_if (fun x -> x < 0)
let raise_notequal v = raise_if (fun x -> x <> v)

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

let socket ~domain ~proto =
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

let size_of_int = Unsigned.Size_t.of_int

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

let send_fd sock =
  let fd = getsockopt_int sock "NN_SOL_SOCKET" "NN_SNDFD" in
  (Obj.magic fd : Unix.file_descr)

let recv_fd sock =
  let fd = getsockopt_int sock "NN_SOL_SOCKET" "NN_RCVFD" in
  (Obj.magic fd : Unix.file_descr)

module B = struct
  let send sock buf pos len =
    if pos < 0 || len < 0 || pos + len > Lwt_bytes.length buf
    then invalid_arg "bounds";
    let nn_buf = nn_allocmsg (size_of_int len) 0 in
    match nn_buf with
    | None -> throw ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      Lwt_bytes.blit buf pos ba 0 len;
      ignore @@ raise_notequal len
        (fun () -> nn_send sock nn_buf_p nn_msg 0)

  let send_from_bytes sock buf pos len =
    if pos < 0 || len < 0 || pos + len > Bytes.length buf
    then invalid_arg "bounds";
    let nn_buf = nn_allocmsg (size_of_int len) 0 in
    match nn_buf with
    | None -> throw ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      Lwt_bytes.blit_from_bytes buf pos ba 0 len;
      ignore @@ raise_notequal len
          (fun () -> nn_send sock nn_buf_p nn_msg 0)

  let send_from_string sock s =
    send_from_bytes sock (Bytes.unsafe_of_string s) 0 (String.length s)

  let recv sock f =
    let open Ctypes in
    let ba_start_p = allocate (ptr void) null in
    let nb_recv =
      raise_negative (fun () -> nn_recv sock ba_start_p nn_msg 0) in
    let ba_start = !@ ba_start_p in
    if nb_recv < 0 then throw ()
    else
      let ba = bigarray_of_ptr array1 nb_recv
          Bigarray.char (from_voidp char ba_start) in
      let res = f ba nb_recv in
      let (_:int) = nn_freemsg ba_start in
      res

  let recv_to_string sock f =
    recv sock (fun ba len ->
        let buf = Bytes.create len in
        Lwt_bytes.blit_to_bytes ba 0 buf 0 len;
        f @@ Bytes.unsafe_to_string buf
      )
end

module NB = struct
  let raise_if sock io_event cond f =
    let open Lwt_unix in
    let fd = match io_event with
      | Write -> send_fd sock
      | Read -> recv_fd sock in
    wrap_syscall io_event (of_unix_file_descr fd) f >>= fun res ->
    if cond res then throw_lwt () else Lwt.return res

  let raise_negative sock io_event f = raise_if sock io_event (fun x -> x < 0) f
  let raise_notequal sock io_event v f = raise_if sock io_event (fun x -> x <> v) f

  let send sock buf pos len =
    if pos < 0 || len < 0 || pos + len > Lwt_bytes.length buf
    then invalid_arg "bounds";
    let nn_buf = nn_allocmsg (size_of_int len) 0 in
    match nn_buf with
    | None -> throw ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      Lwt_bytes.blit buf pos ba 0 len;
      raise_notequal sock Lwt_unix.Write len
        (fun () -> nn_send sock nn_buf_p nn_msg
            Symbol.(value_of_name_exn "NN_DONTWAIT")) >|= fun nb_written ->
      ignore nb_written

  let send_from_bytes sock buf pos len =
    if pos < 0 || len < 0 || pos + len > Bytes.length buf
    then invalid_arg "bounds";
    let nn_buf = nn_allocmsg (size_of_int len) 0 in
    match nn_buf with
    | None -> throw ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      Lwt_bytes.blit_from_bytes buf pos ba 0 len;
      raise_notequal sock Lwt_unix.Write len
        (fun () -> nn_send sock nn_buf_p nn_msg
            Symbol.(value_of_name_exn "NN_DONTWAIT")) >|= fun nb_written ->
      ignore nb_written

  let send_from_string sock s =
    send_from_bytes sock (Bytes.unsafe_of_string s) 0 (String.length s)

  let recv sock f =
    let open Lwt_unix in
    let open Ctypes in
    let ba_start_p = allocate (ptr void) null in
    raise_negative sock Lwt_unix.Read
      (fun () -> nn_recv sock ba_start_p nn_msg
          Symbol.(value_of_name_exn "NN_DONTWAIT")) >>= fun nb_recv ->
    let ba_start = !@ ba_start_p in
    let ba = bigarray_of_ptr array1 nb_recv
        Bigarray.char (from_voidp char ba_start) in
    f ba nb_recv >|= fun res ->
    let (_:int) = nn_freemsg ba_start in
    res

  let recv_to_string sock f =
    recv sock (fun ba len ->
        let buf = Bytes.create len in
        Lwt_bytes.blit_to_bytes ba 0 buf 0 len;
        f @@ Bytes.unsafe_to_string buf
      )
end

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

let int_of_duration = function
  | `Inf -> -1
  | `Ms x -> x

let int_of_bool = function
  | false -> 0
  | true -> 1

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

let set_reconnect_interval sock ival =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_RECONNECT_IVL" ival

let set_send_priority sock priority =
  if priority < 1 || priority > 16 then invalid_arg "set_send_priority";
  setsockopt_int sock "NN_SOL_SOCKET" "NN_SNDPRIO" priority

let set_ipv4_only sock b =
  setsockopt_int sock "NN_SOL_SOCKET" "NN_IPV4ONLY" (int_of_bool b)

let term = nn_term
