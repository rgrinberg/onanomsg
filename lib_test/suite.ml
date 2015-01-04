open Lwt.Infix
open OUnit2

open Nanomsg
module NB = Nanomsg_lwt

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

let socket_test ctx =
  let domains = [AF_SP; AF_SP_RAW] in
  let protos = [Pair; Pub; Sub; Req; Rep; Push; Pull; Surveyor; Respondant; Bus] in
  List.iter
    (fun d ->
       List.iter
         (fun p ->
            let sock = socket ~domain:d p in
            assert_equal d @@ domain sock;
            assert_equal p @@ proto sock;
            assert_equal (`Ms 1000) @@ get_linger sock;
            set_linger sock `Inf;
            assert_equal `Inf @@ get_linger sock;
            set_send_bufsize sock 256;
            set_recv_bufsize sock 256;
            assert_equal 256 @@ get_send_bufsize sock;
            assert_equal 256 @@ get_recv_bufsize sock;
            close sock
         )
         protos
    )
    domains

let send_recv_fd_test ctx =
  let sock = socket Pair in
  ignore @@ recv_fd sock;
  ignore @@ send_fd sock;
  close sock

let pair_test ctx =
  let msgs = ["auie"; "uie,"; "yx.k"] in
  let addr = `Inproc "rdv_point" in
  let peer1 = socket Pair in
  let peer2 = socket Pair in
  let _ = bind peer1 addr in
  let _ = connect peer2 addr in
  let rec inner () =
    Lwt_list.iter_s (fun msg ->
        NB.send_string peer1 msg >>
        let%lwt recv_msg = NB.recv_string peer2 in
        assert_equal msg recv_msg; Lwt.return_unit
      ) msgs >>
    Lwt_list.iter_s (fun msg ->
        NB.send_string peer2 msg >>
        let%lwt recv_msg = NB.recv_string peer1 in
        assert_equal msg recv_msg; Lwt.return_unit
      ) msgs >|= fun () ->
    close peer1;
    close peer2
  in Lwt_main.run @@ inner ()

let reqrep_test ctx =
  let receiver = socket Rep in
  let _ = bind receiver @@ `Inproc "*" in
  let sender = socket Req in
  let _ = connect sender @@ `Inproc "*" in
  let packet = "testing" in
  send_string sender packet;
  let received = recv_string receiver in
  close receiver;
  close sender;
  assert_equal packet received

let pubsub_local_test ctx =
  let address = `Inproc "t2" in
  let sub = socket Sub in
  subscribe sub "";
  let (_:eid) = connect sub address in
  let packet = "foo bar baz" in
  let pub = socket Pub in
  let (_:eid) = bind pub address in
  send_string pub packet;
  let recv_msg = recv_string sub in
  close pub;
  close sub;
  assert_equal packet recv_msg

let pubsub_local_2subs_test ctx =
  let addr1 = `Inproc "tt1" in
  let addr2 = `Inproc "tt2" in
  let sub1 = socket Sub in
  let sub2 = socket Sub in
  let _ = connect sub1 addr1 in
  let _ = connect sub2 addr2 in
  subscribe sub1 "";
  subscribe sub2 "";
  let packet = "one two three" in
  let pub = socket Pub in
  let _ = bind pub addr1 in
  let _ = bind pub addr2 in
  send_string pub packet;
  let x1 = recv_string sub1 in
  let x2 = recv_string sub2 in
  close pub;
  close sub1;
  close sub2;
  assert_equal packet x1;
  assert_equal packet x2

let tcp_pubsub_test ctx =
  let port = 56352 in
  let pub = socket Pub in
  let sub = socket Sub in
  set_ipv4_only pub false;
  set_ipv4_only sub false;
  let _ = bind pub @@ `Tcp (`All, port) in
  let _ = connect sub @@ `Tcp ((`V6 Ipaddr.V6.localhost, None), port) in
  ()

let suite =
  "Nanomsg">:::
  [
    "bind_addr" >:: bind_addr_test;
    "connect_of_string" >:: connect_of_string_test;
    "connect_to_string" >:: connect_to_string_test;
    "socket" >:: socket_test;
    "send_recv_fd" >:: send_recv_fd_test;
    "pair" >:: pair_test;
    "reqrep" >:: reqrep_test;
    "pubsub_local" >:: pubsub_local_test;
    "pubsub_local_2subs" >:: pubsub_local_2subs_test;
    "tcp_pubsub_test" >:: tcp_pubsub_test;
  ]

let () =
  run_test_tt_main suite
