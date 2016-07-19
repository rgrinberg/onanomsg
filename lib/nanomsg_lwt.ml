open Lwt.Infix

open Nanomsg_utils
open Nanomsg

exception Error of string * string

let wrap_error = function
  | Result.Error (name, descr) -> Lwt.fail (Error (name, descr))
  | Result.Ok a -> Lwt.return a

let bind_error f = function
  | Result.Error (name, descr) -> Lwt.fail (Error (name, descr))
  | Result.Ok a -> f a

let map_error f = function
  | Result.Error (name, descr) -> Lwt.fail (Error (name, descr))
  | Result.Ok a -> Lwt.return (f a)

let throw () =
  let code = C.nn_errno () in
  let err_string = C.nn_strerror code in
  let err_value =
    if code > 156384712
    then Symbol.errvalue_of_errno_exn code
    else "" in
  Lwt.fail (Error (err_value, err_string))

let fail_if sock io_event cond f =
  bind_error
    (fun fd ->
       Lwt_unix.(wrap_syscall io_event (of_unix_file_descr fd) f) >>= fun res ->
       if cond res then throw () else Lwt.return res
    )
    (match io_event with
     | Lwt_unix.Write -> send_fd sock
     | Lwt_unix.Read -> recv_fd sock
    )


let fail_negative sock io_event f = fail_if sock io_event (fun x -> x < 0) f
let fail_notequal sock io_event v f = fail_if sock io_event (fun x -> x <> v) f

let send_buf blitf lenf sock buf pos len =
  if pos < 0 || len < 0 || pos + len > lenf buf
  then Lwt.fail (Error ("Internal", "bounds"))
  else
    let nn_buf = C.nn_allocmsg (Unsigned.Size_t.of_int len) 0 in
    match nn_buf with
    | None -> throw ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      blitf buf pos ba 0 len;
      fail_notequal sock Lwt_unix.Write len
        (fun () -> C.nn_send (Obj.magic sock : int)
                     nn_buf_p (Unsigned.Size_t.of_int (-1))
                     Symbol.(value_of_name_exn "NN_DONTWAIT")) >|= fun nb_written ->
      ignore nb_written

let send_bigstring_buf = send_buf Bigstring.blit Bigstring.size
let send_bytes_buf = send_buf Bigstring.blit_of_bytes Bytes.length

let send_bigstring sock buf =
  send_bigstring_buf sock buf 0 @@ Bigstring.size buf

let send_bytes sock b =
  send_bytes_buf sock b 0 (Bytes.length b)

let send_string_buf sock s pos len =
  send_bytes_buf sock (Bytes.unsafe_of_string s) pos len

let send_string sock s =
  send_bytes_buf sock (Bytes.unsafe_of_string s) 0 (String.length s)

let recv sock f =
  let open Lwt_unix in
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  fail_negative sock Lwt_unix.Read
    (fun () -> C.nn_recv (Obj.magic sock : int)
                 ba_start_p (Unsigned.Size_t.of_int (-1))
                 Symbol.(value_of_name_exn "NN_DONTWAIT")) >>= fun nb_recv ->
  let ba_start = !@ ba_start_p in
  let ba = bigarray_of_ptr array1 nb_recv
             Bigarray.char (from_voidp char ba_start) in
  f ba >|= fun res ->
  let (_:int) = C.nn_freemsg ba_start in
  res

let recv_bytes_buf sock buf pos =
  recv sock (fun ba ->
    let len = Bigstring.size ba in
    Bigstring.blit_to_bytes ba 0 buf pos len;
    Lwt.return len
  )

let recv_bytes sock =
  recv sock (fun ba ->
    let len = Bigstring.size ba in
    let buf = Bytes.create len in
    Bigstring.blit_to_bytes ba 0 buf 0 len;
    Lwt.return buf
  )

let recv_string sock = recv_bytes sock >|= Bytes.unsafe_to_string

