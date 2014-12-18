open Lwt.Infix
open OUnit2

open Nanomsg
module NB = Nanomsg_lwt

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
        NB.send_from_string peer1 msg >>
        let%lwt recv_msg = NB.recv_to_string peer2 in
        assert_equal msg recv_msg; Lwt.return_unit
      ) msgs >>
    Lwt_list.iter_s (fun msg ->
        NB.send_from_string peer2 msg >>
        let%lwt recv_msg = NB.recv_to_string peer1 in
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
  send_from_string sender packet;
  let received = recv_to_string receiver in
  close receiver;
  close sender;
  assert_equal packet received

let pubsub_local_test ctx =
  let address = `Inproc "t2" in
  let sub = socket Sub in
  let (_:eid) = connect sub address in
  subscribe sub "";
  let packet = "foo bar baz" in
  let pub = socket Pub in
  let (_:eid) = bind pub address in
  send_from_string pub packet;
  let recv_msg = recv_to_string sub in
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
  send_from_string pub packet;
  let x1 = recv_to_string sub1 in
  let x2 = recv_to_string sub2 in
  close pub;
  close sub1;
  close sub2;
  assert_equal packet x1;
  assert_equal packet x2

let suite =
  "Nanomsg">:::
  [
    "socket" >:: socket_test;
    "send_recv_fd" >:: send_recv_fd_test;
    "pair" >:: pair_test;
    "reqrep" >:: reqrep_test;
    "pubsub_local" >:: pubsub_local_test;
    "pubsub_local_2subs" >:: pubsub_local_2subs_test;
  ]

let () =
  run_test_tt_main suite