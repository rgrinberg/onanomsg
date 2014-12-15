open Onanomsg

let () =
  let address = `Inproc "t2t" in
  let sub = socket ~domain:AF_SP ~proto:Sub in
  (*set_linger sub `Infinite;*)
  let _ = connect sub address in
  set_linger sub (`Milliseconds 2000);
  set_linger sub `Infinite;
  set_send_buffer sub 100;
  set_recv_buffer sub 100;
  subscribe sub "";
  print_endline "Connecting subscriber";
  let packet = "foo bar baz" in
  let pub = socket ~domain:AF_SP ~proto:Pub in
  let _ = bind pub address in
  B.send_from_string pub packet;
  print_endline "published message";
  let recv_msg = B.recv_to_string sub (fun str -> str) in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s\n" recv_msg;
  print_endline "closing publisher";
  close pub;
  print_endline "closing subscriber";
  close sub
