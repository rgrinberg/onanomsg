open Onanomsg

let node0 addr =
  Printf.printf "node0: %s\n%!" @@ string_of_addr addr;
  let s = socket ~domain:AF_SP ~proto:Rep in
  ignore (bind s addr);
  print_endline "starting to listen";
  let msg = B.recv_to_string s
      (fun str -> Printf.printf "NODE0: RECEIVED '%s'\n%!" str; str) in
  B.send_from_string s msg;
  close s

let node1 addr msg =
  Printf.printf "node1: %s\n" @@ string_of_addr addr;
  let s = socket ~domain:AF_SP ~proto:Req in
  let _ = connect s addr in
  Printf.printf "NODE1: SENDING '%s'\n" msg;
  B.send_from_string s msg;
  let recv_msg = B.recv_to_string s (fun str -> str) in
  close s;
  assert (msg = recv_msg)

let () =
  let argc = Array.length Sys.argv in
  if argc > 1 && Sys.argv.(1) = "node0" then
    node0 @@ addr_of_string Sys.argv.(2)
  else if argc > 2 && Sys.argv.(1) = "node1" then
    node1 (addr_of_string Sys.argv.(2)) Sys.argv.(3)
  else
    Printf.printf "Usage: %s node0|node1 <URL> <ARG> ...'\n" Sys.argv.(0)
