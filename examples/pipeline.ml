open Onanomsg
module D = Domain
let printf = Printf.printf

let node0 ~address =
  let s = Socket.socket ~domain:D.Af_sp ~sock_type:Socket.pull in
  ignore (bind s address);
  while true do
    printf "NODE0: RECEIVED '%s'\n" (Onanomsg.recv s);
    flush_all ()
  done

let node1 ~address ~msg =
  let s = Socket.socket ~domain:D.Af_sp ~sock_type:Socket.push in
  let endpoint = connect s ~address in
  printf "NODE1: SENDING '%s'\n" msg;
  Onanomsg.send s msg;
  Onanomsg.shutdown s endpoint

let () =
  let argc = Array.length Sys.argv in
  if argc > 1 && Sys.argv.(1) = "node0" then
    node0 ~address:Sys.argv.(2)
  else if argc > 2 && Sys.argv.(1) = "node1" then
    node1 ~address:Sys.argv.(2) ~msg:Sys.argv.(3)
  else
    printf "Usage: pipeline node0|node1 <URL> <ARG> ...'\n"
