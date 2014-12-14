open Onanomsg

let () =
  let open Domain in
  let open Socket in
  let receiver = socket ~domain:Af_sp ~sock_type:rep in
  let _ = bind receiver @@ `Inproc "*" in
  let sender = socket ~domain:Af_sp ~sock_type:req in
  let _ = connect sender @@ `Inproc "*" in
  let packet = "testing" in
  Printf.printf "packet(%s) length: %d\n" packet (String.length packet);
  send sender packet;
  let received = recv receiver in
  close receiver;
  close sender;
  print_endline ("received: " ^ received);
