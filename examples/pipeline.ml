open Onanomsg
let printf = Printf.printf

let node0 addr =
  let s = socket ~domain:AF_SP ~proto:Pull in
  ignore (bind s addr);
  while true do
    printf "NODE0: RECEIVED '%s'\n" (Onanomsg.recv s);
    flush_all ()
  done

let node1 addr msg =
  let s = socket ~domain:AF_SP ~proto:Push in
  let endpoint = connect s addr in
  printf "NODE1: SENDING '%s'\n" msg;
  Onanomsg.send s msg;
  Onanomsg.shutdown s endpoint

let () =
  let argc = Array.length Sys.argv in
  if argc > 1 && Sys.argv.(1) = "node0" then
    node0 @@ addr_of_string Sys.argv.(2)
  else if argc > 2 && Sys.argv.(1) = "node1" then
    node1 (addr_of_string Sys.argv.(2)) Sys.argv.(3)
  else
    printf "Usage: pipeline node0|node1 <URL> <ARG> ...'\n"
