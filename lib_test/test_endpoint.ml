open Onanomsg

let () =
  let addr1 = `Inproc "tt1" in
  let addr2 = `Inproc "tt2" in
  let (sub1, sub2) = (socket ~domain:AF_SP ~proto:Sub
                 , socket ~domain:AF_SP ~proto:Sub) in
  let (_, _) = (connect sub1 addr1
               , connect sub2 addr2) in
  subscribe sub1 "";
  subscribe sub2 "";
  print_endline "connected subscribers";
  let packet = "one two three" in
  let pub = socket ~domain:AF_SP ~proto:Sub in
  ignore (bind pub addr1);
  ignore (bind pub addr2);
  ignore (send pub packet);
  print_endline "published message";
  let (x1, x2) = (recv sub1, recv sub2) in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s -- Received %s\n" x1 x2;
  print_endline "closed publisher";
  close pub;
  print_endline "closing subscriber";
  (close sub1; close sub2)
