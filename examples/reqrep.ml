open Onanomsg
module D = Domain
let printf = Printf.printf

let node0 addr =
  printf "node0: %s\n" @@ string_of_addr addr;
  flush_all ();
  let s = Socket.socket ~domain:D.Af_sp ~sock_type:Socket.rep in
  ignore (bind s addr);
  print_endline "starting to listen";
  printf "NODE0: RECEIVED '%s'\n" (Onanomsg.recv s);
  print_endline ";(";
  Onanomsg.send s "testing";
  flush_all ()

let node1 addr msg =
  printf "node1: %s\n" @@ string_of_addr addr;
  let s = Socket.socket ~domain:D.Af_sp ~sock_type:Socket.req in
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
    printf "Usage: %s node0|node1 <URL> <ARG> ...'\n" Sys.argv.(0)
