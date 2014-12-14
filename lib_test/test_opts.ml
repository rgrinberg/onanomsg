
let () = 
  let open Onanomsg.Domain in
  let open Onanomsg.Socket in
  let address = `Inproc "t2t" in
  let sub = socket ~domain:Af_sp ~sock_type:sub in
  (*Onanomsg.set_linger sub `Infinite;*)
  let _ = Onanomsg.connect sub address in
  Onanomsg.set_linger sub (`Milliseconds 2000);
  Onanomsg.set_linger sub `Infinite;
  Onanomsg.set_send_buffer sub ~bytes:100;
  Onanomsg.set_recv_buffer sub ~bytes:100;
  Onanomsg.subscribe sub ~topic:"";
  print_endline "Connecting subscriber";
  let packet = "foo bar baz" in
  let pub = socket ~domain:Af_sp ~sock_type:pub in
  let _ = Onanomsg.bind pub address in
  Onanomsg.send pub packet;
  print_endline "published message";
  let recv_msg = Onanomsg.recv sub in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s\n" recv_msg;
  print_endline "closing publisher";
  Onanomsg.close pub;
  print_endline "closing subscriber";
  Onanomsg.close sub
