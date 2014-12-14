open Onanomsg
let printf = Printf.printf

let send sock ~name =
  printf "%s: SENDING \"%s\"\n" name name;
  flush_all ();
  try send sock name;
  (* not sure why E_UNKNOWN is being thrown here *)
  with Error(E_UNKNOWN, _) -> ();
  flush_all ()

let recv sock ~name =
  try
    let buf = recv sock in
    printf "%s: RECEIVED \"%s\"\n" name buf;
    flush_all ()
  (* not sure why E_UNKNOWN is being thrown here *)
  with Error(E_UNKNOWN, _) -> ()

let send_recv sock ~name =
  set_recv_timeout sock (`Milliseconds 100);
  while true do
    recv sock ~name;
    flush_all ();
    Unix.sleep 1;
    send sock ~name;
    flush_all ()
  done

let node0 addr =
  printf "Connecting to address: %s\n" (string_of_addr addr);
  flush_all ();
  let s = socket ~domain:AF_SP ~proto:Pair in
  ignore (bind s addr);
  send_recv s ~name:"node0";
  flush_all ();
  close s

let node1 addr =
  let s = socket ~domain:AF_SP ~proto:Pair in
  ignore (connect s addr);
  send_recv s ~name:"node1";
  close s

let () =
  let argc = Array.length Sys.argv in
  if argc = 3 && Sys.argv.(1) = "node0" then
    node0 @@ addr_of_string Sys.argv.(2)
  else if argc = 3 && Sys.argv.(1) = "node1" then
    node1 @@ addr_of_string Sys.argv.(2)
  else
    printf "Usage: pair node0|node1 <URL> <ARG> ...\n"
