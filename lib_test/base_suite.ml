open OUnit2
open Nanomsg

let bind_addr_test ctx =
  let open Addr in
  assert_equal (`Inproc "9786/+-auieauie7658%=`!!")
    (bind_of_string "inproc://9786/+-auieauie7658%=`!!");
  assert_equal (`Ipc "9786/+-auieauie7658%=`!!")
    (bind_of_string "ipc://9786/+-auieauie7658%=`!!");
  assert_equal (`Tcp (`All,  1234))
    (bind_of_string "tcp://*:1234");
  assert_equal (`Tcp (`V4 Ipaddr.V4.localhost, 1234))
    (bind_of_string "tcp://127.0.0.1:1234");
  assert_equal ~msg:"ipv6" (`Tcp (`V6 Ipaddr.V6.localhost, 1234))
    (bind_of_string "tcp://::1:1234");
  assert_equal ~msg:"ifname" (`Tcp (`Iface "eth0", 1234))
    (bind_of_string "tcp://eth0:1234")

let connect_of_string_test ctx =
  let open Addr in
  assert_equal (`Inproc "9786/+-auieauie7658%=`!!")
    (connect_of_string "inproc://9786/+-auieauie7658%=`!!");
  assert_equal (`Ipc "9786/+-auieauie7658%=`!!")
    (connect_of_string "ipc://9786/+-auieauie7658%=`!!");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"tcp_with_iface"
    (`Tcp ((`V4 Ipaddr.V4.localhost, Some (`Iface "eth0")), 1234))
    (connect_of_string "tcp://eth0;127.0.0.1:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"tcp_without_iface"
    (`Tcp ((`V4 Ipaddr.V4.localhost, None), 1234))
    (connect_of_string "tcp://127.0.0.1:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"dns"
    (`Tcp ((`Dns "localhost", None), 1234))
    (connect_of_string "tcp://localhost:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"::1_none"
    (`Tcp ((`V6 (Ipaddr.V6.localhost), None), 1234))
    (connect_of_string "tcp://::1:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"dns_with_iface"
    (`Tcp ((`Dns "localhost", Some (`Iface "lo0")), 1234))
    (connect_of_string "tcp://lo0;localhost:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"ipv6_iface_with_ipv6_addr"
    (`Tcp ((`V6 (Ipaddr.V6.of_string_exn "dead::beef"), Some (`V6 Ipaddr.V6.localhost)), 1234))
    (connect_of_string "tcp://::1;dead::beef:1234");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"ipv6_addr_wo_iface"
    (`Tcp ((`V6 (Ipaddr.V6.of_string_exn "dead::beef"), None), 1234))
    (connect_of_string "tcp://dead::beef:1234")

let connect_to_string_test ctx =
  let open Addr in
  assert_equal
    (connect_of_string "inproc://9786/+-auieauie7658%=`!!")
    (`Inproc "9786/+-auieauie7658%=`!!");
  assert_equal
    (connect_of_string "ipc://9786/+-auieauie7658%=`!!")
    (`Ipc "9786/+-auieauie7658%=`!!");
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"tcp_with_iface"
    (connect_of_string "tcp://eth0;127.0.0.1:1234")
    (`Tcp ((`V4 Ipaddr.V4.localhost, Some (`Iface "eth0")), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"tcp_without_iface"
    (connect_of_string "tcp://127.0.0.1:1234")
    (`Tcp ((`V4 Ipaddr.V4.localhost, None), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"dns"
    (connect_of_string "tcp://localhost:1234")
    (`Tcp ((`Dns "localhost", None), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"::1_none"
    (connect_of_string "tcp://::1:1234")
    (`Tcp ((`V6 (Ipaddr.V6.localhost), None), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"dns_with_iface"
    (connect_of_string "tcp://lo0;localhost:1234")
    (`Tcp ((`Dns "localhost", Some (`Iface "lo0")), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"ipv6_iface_with_ipv6_addr"
    (connect_of_string "tcp://::1;dead::beef:1234")
    (`Tcp ((`V6 (Ipaddr.V6.of_string_exn "dead::beef"), Some (`V6 Ipaddr.V6.localhost)), 1234));
  assert_equal
    ~printer:(Addr.show Addr.pp_connect) ~msg:"ipv6_addr_wo_iface"
    (connect_of_string "tcp://dead::beef:1234")
    (`Tcp ((`V6 (Ipaddr.V6.of_string_exn "dead::beef"), None), 1234))

let (>>=?) m msg f =
  match m with
  | `Ok v  -> f v
  | `Error (s1, s2) ->
    assert_failure (Printf.sprintf "%s: '%s' '%s'" msg s1 s2)

let socket_test ctx =
  let domains = [AF_SP; AF_SP_RAW] in
  let protos = [Pair; Pub; Sub; Req; Rep; Push; Pull; Surveyor; Respondant; Bus] in
  List.iter
    (fun d ->
       List.iter
         (fun p ->
            let open CCError in
            CCError.catch
              (socket ~domain:d p >>= fun sock ->
               domain sock >>= fun sock_domain ->
               proto sock >>= fun sock_proto ->
               get_linger sock >>= fun sock_linger ->
               assert_equal d sock_domain;
               assert_equal p sock_proto;
               assert_equal (`Ms 1000) sock_linger;
               set_linger sock `Inf >>= fun () ->
               get_linger sock >>= fun sock_linger ->
               assert_equal `Inf sock_linger;
               set_send_bufsize sock 256 >>= fun () ->
               set_recv_bufsize sock 256 >>= fun () ->
               get_send_bufsize sock >>= fun send_bufsize ->
               get_recv_bufsize sock >>= fun recv_bufsize ->
               assert_equal 256 send_bufsize;
               assert_equal 256 recv_bufsize;
               close sock)
              ~ok:(fun () -> ())
              ~err:(fun (e, m) -> failwith m);
         )
         protos
    ) domains

let device_test ctx =
  (* int s1 = nn_socket (AF_SP_RAW, NN_REQ); *)
  (* nn_bind (s1, "tcp://eth0:5555"); *)
  (* int s2 = nn_socket (AF_SP_RAW, NN_REP); *)
  (* nn_bind (s2, "tcp://eth0:5556"); *)
  (* nn_device (s1, s2); *)
  let open CCError in
  get_exn
    (socket ~domain:AF_SP_RAW Req >>= fun s1 ->
     bind s1 (`Tcp (`All, 5555)) >>= fun eid1 ->
     socket ~domain:AF_SP_RAW Rep >>= fun s2 ->
     bind s2 (`Tcp (`All, 5556)) >>= fun eid2 ->
     device s1 s2
    )

let send_recv_fd_test ctx =
  let sock = socket_exn Pair in
  ignore @@ recv_fd sock;
  ignore @@ send_fd sock;
  close_exn sock

let reqrep_test ctx =
  let open CCError in
  let receiver = socket_exn Rep in
  let sender = socket_exn Req in
  let _ = bind_exn receiver @@ `Inproc "*" in
  let _ = connect_exn sender @@ `Inproc "*" in
  let packet = "testing" in
  send_string sender packet >>= fun () ->
  recv_string receiver >>= fun received ->
  close receiver >>= fun () ->
  close sender >|= fun () ->
  assert_equal packet received

let pubsub_local_test ctx =
  let open CCError in
  let address = `Inproc "t2" in
  socket Sub >>= fun sub ->
  subscribe sub "" >>= fun () ->
  connect sub address >>= fun _ ->
  let packet = "foo bar baz" in
  socket Pub >>= fun pub ->
  bind pub address >>= fun _ ->
  send_string pub packet >>= fun _ ->
  recv_string sub >>= fun recv_msg ->
  close pub >>= fun _ ->
  close sub >|= fun _ ->
  assert_equal packet recv_msg

let pubsub_local_2subs_test ctx =
  let open CCError in
  let addr1 = `Inproc "tt1" in
  let addr2 = `Inproc "tt2" in
  socket Sub >>= fun sub1 ->
  socket Sub >>= fun sub2 ->
  let _ = connect sub1 addr1 in
  let _ = connect sub2 addr2 in
  subscribe sub1 "" >>= fun () ->
  subscribe sub2 "" >>= fun () ->
  let packet = "one two three" in
  socket Pub >>= fun pub ->
  let _ = bind pub addr1 in
  let _ = bind pub addr2 in
  send_string pub packet >>= fun () ->
  recv_string sub1 >>= fun x1 ->
  recv_string sub2 >>= fun x2 ->
  close pub >>= fun () ->
  close sub1 >>= fun () ->
  close sub2 >|= fun () ->
  assert_equal packet x1;
  assert_equal packet x2

let suite =
  "Nanomsg">:::
  [
    "bind_addr" >:: bind_addr_test;
    "connect_of_string" >:: connect_of_string_test;
    "connect_to_string" >:: connect_to_string_test;
    "socket" >:: socket_test;
    "send_recv_fd" >:: send_recv_fd_test;
    "reqrep" >:: (fun a -> CCError.get_exn @@ reqrep_test a);
    "pubsub_local" >:: (fun a -> CCError.get_exn @@ pubsub_local_test a);
    "pubsub_local_2subs" >:: (fun a -> CCError.get_exn @@ pubsub_local_2subs_test a);
  ]

let () =
  run_test_tt_main suite
