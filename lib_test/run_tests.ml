
let () =
  let sock = Onanomsg.socket ~domain:Onanomsg.Af_sp ~sock_type:`Req in
  let endpoint = Onanomsg.bind sock ~transport:"inproc://test" in
  Onanomsg.close sock


