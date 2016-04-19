open Lwt.Infix
open OUnit

open Nanomsg
module NB = Nanomsg_lwt

let reqrep_test _ =
  let open Nanomsg_utils.Res in
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

let ok msg = function
  | Result.Error m -> assert_failure msg
  | Result.Ok v -> v

let tcp_pubsub_test _ =
  let open Nanomsg_lwt in
  let inner () =
    let port = 56352 in
    wrap_error @@ socket Pub >>= fun pub ->
    wrap_error @@ socket Sub >>= fun sub ->
    wrap_error @@ set_ipv4_only pub false >>= fun () ->
    wrap_error @@ set_ipv4_only sub false >>= fun () ->
    let _ = ok "tcp_pubsub: bind" @@ bind pub @@ `Tcp (`All, port) in
    let _ = ok "tcp_pubsub: connect" @@ connect sub @@
      `Tcp ((`V6 Ipaddr.V6.localhost, None), port) in
    wrap_error @@ Nanomsg.subscribe sub "" >>= fun () ->
    let msg = "bleh" in
    let len = String.length msg in
    let recv_msg = Bytes.create @@ String.length msg in
    let recv_msg' = Bytes.create @@ String.length msg in
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

let pipeline_local_test _ =
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
    [ sender (`Inproc "rdvpoint")
    ; receiver (`Inproc "rdvpoint") ]

let suite =
  "Nanomsg">:::
  [ "reqrep" >:: (fun a -> Nanomsg_utils.Res.get_exn @@ reqrep_test a)
  ; "tcp_pubsub" >:: (fun a -> Nanomsg_utils.Res.get_exn @@ tcp_pubsub_test a)
  ; "pipeline_local" >:: pipeline_local_test ]

let () = ignore (run_test_tt_main suite)
