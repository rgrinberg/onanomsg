open Onanomsg

let () =
  let open Domain in
  let open Socket in
  let address = `Inproc "t2" in
  let sub = socket ~domain:Af_sp ~sock_type:sub in
  let (_:endpoint) = connect sub address in
  subscribe sub ~topic:"";
  print_endline "Connecting subscriber";
  let packet = "foo bar baz" in
  let pub = socket ~domain:Af_sp ~sock_type:pub in
  let (_:endpoint) = bind pub address in
  send pub packet;
  print_endline "published message";
  let recv_msg = recv sub in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s\n" recv_msg;
  print_endline "closing publisher";
  close pub;
  print_endline "closing subscriber";
  close sub
