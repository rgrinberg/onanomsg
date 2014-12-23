open Nanomsg

let node0 addr =
  let s = socket Pull in
  ignore (bind s addr);
  while true do
    let msg = recv_to_string s in
    Printf.printf "NODE0: RECEIVED '%s'\n%!" msg
  done

let node1 addr msg =
  let s = socket Push in
  let endpoint = connect s addr in
  Printf.printf "NODE1: SENDING '%s'\n%!" msg;
  send_from_string s msg;
  shutdown s endpoint

let () =
  let argc = Array.length Sys.argv in
  if argc > 1 && Sys.argv.(1) = "node0" then
    node0 @@ Addr.bind_of_string Sys.argv.(2)
  else if argc > 2 && Sys.argv.(1) = "node1" then
    node1 (Addr.connect_of_string Sys.argv.(2)) Sys.argv.(3)
  else
    Printf.eprintf "Usage: %s node0|node1 <URL> <ARG> ...'\n" Sys.argv.(0)
