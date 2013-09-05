
let () =
  let open Onanomsg.Domain in
  let open Onanomsg.Socket in
  let receiver = socket ~domain:Af_sp ~sock_type:rep in
  let _ = Onanomsg.bind receiver ~address:"inproc://*" in
  let sender = socket ~domain:Af_sp ~sock_type:req in
  let _ = Onanomsg.connect sender ~address:"inproc://*" in
  let packet = "testing" in
  Printf.printf "packet(%s) length: %d\n" packet (String.length packet);
  let (`Read _) = Onanomsg.send sender packet in
  let received = Onanomsg.recv receiver in
  Onanomsg.close receiver;
  Onanomsg.close sender;
  print_endline ("received: " ^ received);
