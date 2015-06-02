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
            let open CCError in
            get_exn
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
         )
         protos
    )
    domains

let send_recv_fd_test ctx =
  let sock = socket_exn Pair in
  ignore @@ recv_fd sock;
  ignore @@ send_fd sock;
  close_exn sock

let pair_test ctx =
  let msgs = ["auie"; "uie,"; "yx.k"] in
  let addr = `Inproc "rdv_point" in
  let peer1 = socket_exn Pair in
  let peer2 = socket_exn Pair in
  let _ = bind peer1 addr in
  let _ = connect peer2 addr in
  let rec inner () =
    Lwt_list.iter_s (fun msg ->
        NB.send_string peer1 msg >>= fun () ->
        NB.recv_string peer2 >>= fun recv_msg ->
        assert_equal msg recv_msg; Lwt.return_unit
      ) msgs >>= fun () ->
    Lwt_list.iter_s (fun msg ->
        NB.send_string peer2 msg >>= fun () ->
        NB.recv_string peer1 >>= fun recv_msg ->
        assert_equal msg recv_msg; Lwt.return_unit
      ) msgs >|= fun () ->
    close_exn peer1;
    close_exn peer2
  in Lwt_main.run @@ inner ()

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

let tcp_pubsub_test ctx =
  let open Nanomsg_lwt in
  let inner () =
    let port = 56352 in
    wrap_error @@ socket Pub >>= fun pub ->
    wrap_error @@ socket Sub >>= fun sub ->
    wrap_error @@ set_ipv4_only pub false >>= fun () ->
    wrap_error @@ set_ipv4_only sub false >>= fun () ->
    let _ = bind pub @@ `Tcp (`All, port) in
    let _ = connect sub @@ `Tcp ((`V6 Ipaddr.V6.localhost, None), port) in
    wrap_error @@ Nanomsg.subscribe sub "" >>= fun () ->
    let msg = "bleh" in
    let len = String.length msg in
    let recv_msg = Bytes.create @@ String.length msg in
    let recv_msg' = Bytes.create @@ String.length msg in
    let open Lwt.Infix in
    let th = Nanomsg_lwt.send_string pub msg in
    Nanomsg_lwt.recv_string sub >>= fun str ->
    assert_equal (Lwt.Return ()) (Lwt.state th);
    assert_equal msg str;
    let th = Nanomsg_lwt.send_string_buf pub msg 0 len in
    Nanomsg_lwt.recv_bytes_buf sub recv_msg 0 >>= fun (_:int) ->
    assert_equal (Lwt.Return ()) (Lwt.state th);
    assert_equal msg (Bytes.unsafe_to_string recv_msg);
    let th = Nanomsg_lwt.send_bytes pub recv_msg in
    Nanomsg_lwt.recv_bytes_buf sub recv_msg' 0 >|= fun (_:int) ->
    assert_equal (Lwt.Return ()) (Lwt.state th);
    assert_equal recv_msg recv_msg';
    close_exn pub;
    close sub
  in Lwt_main.run @@ inner ()

let pipeline_local_test ctx =
  let open Nanomsg_lwt in
  let msgs = [|"foo"; "bar"; "baz"|] in
  let receiver addr =
    wrap_error @@ socket Pull >>= fun s ->
    wrap_error @@ bind s addr >>= fun _ ->
    let rec inner n =
      if n > 2
      then wrap_error @@ close s
      else
        Nanomsg_lwt.recv_string s >>= fun m ->
        (assert_equal msgs.(n) m; inner (succ n))
    in
    inner 0
  in
  let sender addr =
    wrap_error @@ socket Push >>= fun s ->
    wrap_error @@ connect s addr >>= fun _ ->
    Lwt_list.iter_s (Nanomsg_lwt.send_string s) @@ Array.to_list msgs >>= fun () ->
    Lwt_unix.yield () >>= fun () -> wrap_error @@ close s
  in
  Lwt_main.run @@
    Lwt.join
      [
        sender (`Inproc "rdvpoint");
        receiver (`Inproc "rdvpoint")
      ]

let suite =
  "Nanomsg">:::
  [
    "bind_addr" >:: bind_addr_test;
    "connect_of_string" >:: connect_of_string_test;
    "connect_to_string" >:: connect_to_string_test;
    "socket" >:: socket_test;
    "send_recv_fd" >:: send_recv_fd_test;
    "pair" >:: pair_test;
    "reqrep" >:: (fun a -> CCError.get_exn @@ reqrep_test a);
    "pubsub_local" >:: (fun a -> CCError.get_exn @@ pubsub_local_test a);
    "pubsub_local_2subs" >:: (fun a -> CCError.get_exn @@ pubsub_local_2subs_test a);
    "tcp_pubsub" >:: (fun a -> CCError.get_exn @@ tcp_pubsub_test a);
    "pipeline_local" >:: pipeline_local_test;
  ]

let () =
  run_test_tt_main suite
