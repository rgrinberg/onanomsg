

let () = 
  let (addr1, addr2) = ("inproc://tt1", "inproc://tt2") in
  let (r1, r2) = (Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Sub
                 , Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Sub) in
  let (_, _) = (Onanomsg.connect r1 ~address:addr1
               , Onanomsg.connect r2 ~address:addr2) in
  print_endline "connected subscribers";
  let packet = "one two three" in
  let sub = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Pub in
  ignore (Onanomsg.bind sub ~address:addr1);
  ignore (Onanomsg.bind sub ~address:addr2);
  ignore (Onanomsg.send sub packet);
  print_endline "published message";
  Onanomsg.close sub;
  let (x1, x2) = Onanomsg.(recv r1, recv r2) in
  Printf.printf "Received: %s -- Received %s\n" x1 x2;
  Onanomsg.(close r1; close r2)
