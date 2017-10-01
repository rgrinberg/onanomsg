open Core
open Async

module Bytes = Caml.Bytes

open Nanomsg_utils
open Nanomsg

let ready sock io_event =
  let f = match io_event with
    | `Write -> send_fd
    | `Read -> recv_fd
  in
  f sock |> function
  | Error _ -> return @@ `Bad_fd
  | Ok fd ->
    let fd = Fd.create ~avoid_nonblock_if_possible:true
        (Fd.Kind.Socket `Passive) fd
        Info.(of_string "nanomsg pollfd") in
    Fd.ready_to fd io_event

let send_buf blitf lenf sock buf pos len =
  if pos < 0 || len < 0 || pos + len > lenf buf
  then return @@ Result.Error ("Internal", "bounds")
  else
    C.nn_allocmsg (Unsigned.Size_t.of_int len) 0 |> function
    | None -> return @@ error ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      blitf ~src:buf ~src_pos:pos ~dst:ba ~dst_pos:0 ~len;
      ready sock `Write >>| function
      | `Bad_fd | `Closed -> Result.Error ("Internal", "`Bad_fd | `Closed")
      | `Ready ->
        let _ =
          C.nn_send (Obj.magic sock : int)
            nn_buf_p (Unsigned.Size_t.of_int (-1))
            Symbol.(value_of_name_exn "NN_DONTWAIT") in
        Result.Ok ()

let send_bigstring_buf = send_buf Bigstring.blit Bigstring.length
let send_bytes_buf = send_buf Bigstring.From_string.blit Bytes.length

let send_bigstring sock buf =
  send_bigstring_buf sock buf 0 @@ Bigstring.length buf

let send_bytes sock b =
  send_bytes_buf sock b 0 (Bytes.length b)

let send_string_buf sock s pos len =
  send_bytes_buf sock (Bytes.unsafe_of_string s) pos len

let send_string sock s =
  send_bytes_buf sock (Bytes.unsafe_of_string s) 0 (String.length s)

let recv sock f =
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  ready sock `Read >>= function
  | `Bad_fd | `Closed ->
    return @@ Result.Error ("Internal", "`Bad_fd | `Closed")
  | `Ready ->
    let nb_recv = C.nn_recv (Obj.magic sock : int)
        ba_start_p (Unsigned.Size_t.of_int (-1))
        Symbol.(value_of_name_exn "NN_DONTWAIT") in
    let ba_start = !@ ba_start_p in
    let ba = bigarray_of_ptr array1 nb_recv
        Bigarray.char (from_voidp char ba_start) in
    f ba >>| fun res ->
    let (_:int) = C.nn_freemsg ba_start in
    Result.Ok res

let recv_bytes_buf sock buf pos =
  recv sock (fun ba ->
      let len = Bigstring.length ba in
      Bigstring.To_string.blit ba 0 buf pos len;
      return len
    )

let recv_bytes sock =
  recv sock (fun ba ->
      let len = Bigstring.length ba in
      let buf = Bytes.create len in
      Bigstring.To_string.blit ~src:ba ~src_pos:0 ~dst:buf ~dst_pos:0 ~len;
      return buf
    )

let recv_string sock =
  recv_bytes sock >>| Nanomsg_utils.Res.map Bytes.unsafe_to_string

