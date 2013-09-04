open Nanomsg

exception General_error of string

type domains =
  | Af_sp
  | Af_sp_raw

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

type 'a sock = Socket of int

type endpoint = Endpoint of int

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

let current_error () = nn_strerror (nn_errno ())

let throw_current_error () = raise (General_error (current_error ()))

let raise_if ~cond v = if cond v then throw_current_error ()
let raise_negative = raise_if ~cond:(fun x -> x < 0)
let raise_not_zero = raise_if ~cond:(fun x -> x <> 0)

let int_of_domain = function
  | Af_sp -> af_sp
  | Af_sp_raw -> af_sp_raw

let socket ?(domain=Af_sp) ~sock_type =
  let ret = nn_socket (int_of_domain domain) (int_of_sock_type sock_type) in
  raise_negative ret;
  Socket ret

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
  raise_negative read;
  `Read read

let recv ?(block=true) (Socket socket) =
  let open Ctypes in
  let flag = if block then 0 else nn_dontwait in
  let s = allocate string_opt None in
  let read = nn_recv socket s nn_msg flag in
  raise_negative read;
  match !@ s with
  | None -> failwith "TEMP STUFF"
  | Some x -> x
