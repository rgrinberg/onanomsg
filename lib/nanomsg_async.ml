open Core.Std
open Async.Std

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

module type STRING = sig
  type t
  val length : t -> int
  include Blit.S_distinct with type src = t and type dst = Bigstring.t
end

let send_buf (type t) (module S : STRING with type t = t) ?(pos=0) ?len sock buf =
  let buflen = S.length buf in
  let len = Option.value len ~default:buflen in
  if pos < 0 || len < 0 || pos + len > buflen
  then return @@ Result.fail ("Internal", "bounds")
  else
    C.nn_allocmsg (Unsigned.Size_t.of_int len) 0 |> function
    | None -> return @@ error ()
    | Some nn_buf ->
      let nn_buf_p = Ctypes.(allocate (ptr void) nn_buf) in
      let ba = Ctypes.(bigarray_of_ptr array1 len
                         Bigarray.char @@ from_voidp char nn_buf) in
      S.blit buf pos ba 0 len;
      ready sock `Write >>| function
      | `Bad_fd | `Closed -> Result.fail ("Internal", "`Bad_fd | `Closed")
      | `Ready ->
        let _ =
          C.nn_send (Obj.magic sock : int)
            nn_buf_p (Unsigned.Size_t.of_int (-1))
            Symbol.(value_of_name_exn "NN_DONTWAIT") in
        Result.return ()

let recv sock =
  let open Ctypes in
  let ba_start_p = allocate (ptr void) null in
  ready sock `Read >>| function
  | `Bad_fd | `Closed ->
    Result.fail ("Internal", "`Bad_fd | `Closed")
  | `Ready ->
    let nb_recv = C.nn_recv (Obj.magic sock : int)
        ba_start_p (Unsigned.Size_t.of_int (-1))
        Symbol.(value_of_name_exn "NN_DONTWAIT") in
    let ba_start = !@ ba_start_p in
    let ba = bigarray_of_ptr array1 nb_recv
        Bigarray.char (from_voidp char ba_start) in
    let (_:int) = C.nn_freemsg ba_start in
    Result.return ba

module String = struct
  module S = struct
    include String
    type src = string
    type dst = Bigstring.t
    include Bigstring.From_string
  end

  let send = send_buf (module S : STRING with type t = String.t)
  let recv ?(pos=0) ?len sock buf =
    let maxlen = String.length buf - pos in
    let maxlen = Option.value_map len ~default:maxlen ~f:(fun l -> Int.min l maxlen) in
    recv sock >>| function
    | Error err -> Error err
    | Ok ba ->
      let len = Int.min maxlen (Bigstring.length ba) in
      Bigstring.To_string.blit ~src:ba ~src_pos:0 ~dst:buf ~dst_pos:pos ~len;
      Result.return ()
end

module Bigstring = struct
  module BS = struct
    include Bigstring
    type src = t
    type dst = t
  end

  let send = send_buf (module BS : STRING with type t = Bigstring.t)
  let recv = recv
end
