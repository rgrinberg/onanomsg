open Onanomsg

let () =
  let receiver = socket ~domain:AF_SP ~proto:Rep in
  let _ = bind receiver @@ `Inproc "*" in
  let sender = socket ~domain:AF_SP ~proto:Req in
  let _ = connect sender @@ `Inproc "*" in
  let packet = "testing" in
  Printf.printf "packet(%s) length: %d\n" packet (String.length packet);
  send sender packet;
  let received = recv receiver in
  close receiver;
  close sender;
  print_endline ("received: " ^ received);
