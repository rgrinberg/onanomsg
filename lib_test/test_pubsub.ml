
let () = 
  let address = "inproc://t2t" in
  let sub = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Sub in
  let endpoint = Onanomsg.connect sub ~address in
  Onanomsg.subscribe sub ~topic:"";
  print_endline "Connecting subscriber";
  let packet = "foo bar baz" in
  let pub = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Pub in
  let endpoint = Onanomsg.bind pub ~address in
  let (`Read _) = Onanomsg.send pub packet in
  print_endline "published message";
  let recv_msg = Onanomsg.recv sub in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s\n" recv_msg;
  print_endline "closing publisher";
  Onanomsg.close pub;
  print_endline "closing subscriber";
  Onanomsg.close sub
