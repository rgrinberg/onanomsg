

let () = 
  let (addr1, addr2) = ("inproc://tt1", "inproc://tt2") in
  let (sub1, sub2) = (Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Sub
                 , Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Sub) in
  let (_, _) = (Onanomsg.connect sub1 ~address:addr1
               , Onanomsg.connect sub2 ~address:addr2) in
  Onanomsg.subscribe sub1 ~topic:"";
  Onanomsg.subscribe sub2 ~topic:"";
  print_endline "connected subscribers";
  let packet = "one two three" in
  let pub = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Pub in
  ignore (Onanomsg.bind pub ~address:addr1);
  ignore (Onanomsg.bind pub ~address:addr2);
  ignore (Onanomsg.send pub packet);
  print_endline "published message";
  let (x1, x2) = Onanomsg.(recv sub1, recv sub2) in
  print_endline "receiving in subscribers";
  Printf.printf "Received: %s -- Received %s\n" x1 x2;
  print_endline "closed publisher";
  Onanomsg.close pub;
  print_endline "closing subscriber";
  Onanomsg.(close sub1; close sub2)
