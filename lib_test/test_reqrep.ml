open Onanomsg

let () =
  let receiver = socket ~domain:AF_SP ~proto:Rep in
  let _ = bind receiver @@ `Inproc "*" in
  let sender = socket ~domain:AF_SP ~proto:Req in
  let _ = connect sender @@ `Inproc "*" in
  let packet = "testing" in
  Printf.printf "packet(%s) length: %d\n" packet (String.length packet);
  B.send_from_string sender packet;
  let received = B.recv_to_string receiver (fun str -> str) in
  close receiver;
  close sender;
  print_endline ("received: " ^ received);
