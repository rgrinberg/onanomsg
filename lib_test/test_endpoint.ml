open Onanomsg

let () =
  let addr1 = `Inproc "tt1" in
  let addr2 = `Inproc "tt2" in
  let sub1 = socket ~domain:AF_SP ~proto:Sub in
  let sub2 = socket ~domain:AF_SP ~proto:Sub in
  ignore @@ connect sub1 addr1;
  ignore @@ connect sub2 addr2;
  subscribe sub1 "";
  subscribe sub2 "";
  print_endline "connected subscribers";
  let packet = "one two three" in
  let pub = socket ~domain:AF_SP ~proto:Pub in
  ignore (bind pub addr1);
  ignore (bind pub addr2);
  ignore (B.send_from_string pub packet);
  print_endline "published message";
  let x1 = B.recv_to_string sub1 (fun str -> str) in
  let x2 = B.recv_to_string sub2 (fun str -> str) in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s -- Received %s\n" x1 x2;
  print_endline "closed publisher";
  close pub;
  print_endline "closing subscriber";
  close sub1; close sub2
