open Lwt.Infix

open Nanomsg_ctypes
open Nanomsg_utils
open Nanomsg

let throw () =
  let code = nn_errno () in
  let err_string = nn_strerror code in
  let err_value =
    if code > 156384712
    then Symbol.errvalue_of_errno_exn code
    else "" in
  Lwt.fail (Error (err_value, err_string))

let raise_if sock io_event cond f =
  let open Lwt_unix in
  let fd = match io_event with
    | Write -> send_fd sock
    | Read -> recv_fd sock in
  wrap_syscall io_event (of_unix_file_descr fd) f >>= fun res ->
  if cond res then throw () else Lwt.return res

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
      (fun () -> nn_send (Obj.magic sock : int) nn_buf_p nn_msg
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
      (fun () -> nn_send (Obj.magic sock : int)  nn_buf_p nn_msg
          Symbol.(value_of_name_exn "NN_DONTWAIT")) >|= fun nb_written ->
    ignore nb_written

let send_from_string_raw sock s pos len =
  send_from_bytes sock (Bytes.unsafe_of_string s) pos len

let send_from_string sock s =
  send_from_bytes sock (Bytes.unsafe_of_string s) 0 (String.length s)

let recv sock f =
  let open Lwt_unix in
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  raise_negative sock Lwt_unix.Read
    (fun () -> nn_recv (Obj.magic sock : int) ba_start_p nn_msg
        Symbol.(value_of_name_exn "NN_DONTWAIT")) >>= fun nb_recv ->
  let ba_start = !@ ba_start_p in
  let ba = bigarray_of_ptr array1 nb_recv
      Bigarray.char (from_voidp char ba_start) in
  f ba >|= fun res ->
  let (_:int) = nn_freemsg ba_start in
  res

let recv_to_string sock =
  recv sock (fun ba ->
      let len = Lwt_bytes.length ba in
      let buf = Bytes.create len in
      Lwt_bytes.blit_to_bytes ba 0 buf 0 len;
      Lwt.return @@ Bytes.unsafe_to_string buf
    )
