
let () =
  let receiver = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Rep in
  let _ = Onanomsg.bind receiver ~address:"inproc://test" in
  let sender = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Req in
  let _ = Onanomsg.connect sender ~address:"inproc://test" in
  let packet = "testing" in
  ignore (Onanomsg.send sender packet);
  let buf = String.create (String.length packet) in
  let received = Onanomsg.recv_str receiver ~str:buf in
  Onanomsg.close receiver;
  Onanomsg.close sender;
  print_endline ("received: " ^ buf)


