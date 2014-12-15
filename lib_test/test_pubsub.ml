open Onanomsg

let () =
  let address = `Inproc "t2" in
  let sub = socket ~domain:AF_SP ~proto:Sub in
  let (_:eid) = connect sub address in
  subscribe sub "";
  print_endline "Connecting subscriber";
  let packet = "foo bar baz" in
  let pub = socket ~domain:AF_SP ~proto:Pub in
  let (_:eid) = bind pub address in
  B.send_from_string pub packet;
  print_endline "published message";
  let recv_msg = B.recv_to_string sub (fun str -> str) in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s\n" recv_msg;
  print_endline "closing publisher";
  close pub;
  print_endline "closing subscriber";
  close sub
