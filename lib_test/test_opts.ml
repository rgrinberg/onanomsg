open Onanomsg

let () =
  let address = `Inproc "t2t" in
  let sub = socket ~domain:AF_SP ~proto:Sub in
  (*set_linger sub `Infinite;*)
  let _ = connect sub address in
  set_linger sub (`Milliseconds 2000);
  set_linger sub `Infinite;
  set_send_buffer sub ~bytes:100;
  set_recv_buffer sub ~bytes:100;
  subscribe sub ~topic:"";
  print_endline "Connecting subscriber";
  let packet = "foo bar baz" in
  let pub = socket ~domain:AF_SP ~proto:Pub in
  let _ = bind pub address in
  send pub packet;
  print_endline "published message";
  let recv_msg = recv sub in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s\n" recv_msg;
  print_endline "closing publisher";
  close pub;
  print_endline "closing subscriber";
  close sub
