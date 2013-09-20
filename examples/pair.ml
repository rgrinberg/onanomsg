open Onanomsg
module D = Domain
let printf = Printf.printf

let send sock ~name =
  printf "%s: SENDING \"%s\"\n" name name;
  flush_all ();
  try Onanomsg.send sock name;
  (* not sure why E_UNKNOWN is being thrown here *)
  with Onanomsg.Error(Onanomsg.E_UNKNOWN, _) -> ();
  flush_all ()

let recv sock ~name =
  try
    let buf = Onanomsg.recv sock in
    printf "%s: RECEIVED \"%s\"\n" name buf;
    flush_all ()
  (* not sure why E_UNKNOWN is being thrown here *)
  with Onanomsg.Error(Onanomsg.E_UNKNOWN, _) -> ()

let send_recv sock ~name =
  Onanomsg.set_recv_timeout sock (`Milliseconds 100);
  while true do
    recv sock ~name;
    flush_all ();
    Unix.sleep 1;
    send sock ~name;
    flush_all ()
  done

let node0 ~address =
  printf "Connecting to address: %s\n" address;
  flush_all ();
  let s = Socket.socket ~domain:D.Af_sp ~sock_type:Socket.pair in
  ignore (Onanomsg.bind s address);
  send_recv s ~name:"node0";
  flush_all ();
  Onanomsg.close s

let node1 ~address =
  let s = Socket.socket ~domain:D.Af_sp ~sock_type:Socket.pair in
  ignore (Onanomsg.connect s address);
  send_recv s ~name:"node1";
  Onanomsg.close s

let () =
  let argc = Array.length Sys.argv in
  if argc = 3 && Sys.argv.(1) = "node0" then
    node0 ~address:Sys.argv.(2)
  else if argc = 3 && Sys.argv.(1) = "node1" then
    node1 ~address:Sys.argv.(2)
  else
    printf "Usage: pair node0|node1 <URL> <ARG> ...\n"
