open Nanomsg

let node0 addr =
  Printf.printf "node0: %s\n%!" addr;
  let s = socket Rep in
  ignore (bind s @@ Addr.bind_of_string addr);
  print_endline "starting to listen";
  let msg = recv_to_string s in
  Printf.printf "NODE0: RECEIVED '%s'\n%!" msg;
  send_from_string s msg;
  close s

let node1 addr msg =
  Printf.printf "node1: %s\n" addr;
  let s = socket Req in
  let _ = connect s @@ Addr.connect_of_string addr in
  Printf.printf "NODE1: SENDING '%s'\n" msg;
  send_from_string s msg;
  let recv_msg = recv_to_string s in
  close s;
  assert (msg = recv_msg)

let () =
  let argc = Array.length Sys.argv in
  if argc > 1 && Sys.argv.(1) = "node0" then
    node0 Sys.argv.(2)
  else if argc > 2 && Sys.argv.(1) = "node1" then
    node1 Sys.argv.(2) Sys.argv.(3)
  else
    Printf.printf "Usage: %s node0|node1 <URL> <ARG> ...'\n" Sys.argv.(0)
