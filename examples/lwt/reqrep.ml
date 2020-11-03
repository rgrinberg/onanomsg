open Lwt

let requester () =
    Lwt_log.warning_f "Starting requester..." >>
    let sock = Nanomsg.socket_exn Nanomsg.Req in
    let _ = Nanomsg.connect_exn sock (Nanomsg.Addr.connect_of_string "inproc://test") in
    while%lwt true
    do
        let msg_req = string_of_int (Random.int 100) in
        Nanomsg_lwt.send_string sock msg_req >>
        Lwt_log.warning_f "Requester sent '%s'." msg_req >>
        Nanomsg_lwt.recv_string sock >>= fun msg_rep ->
        Lwt_log.warning_f "Requester received '%s' as reply to '%s'." msg_rep msg_req >>
        Lwt_unix.sleep 1.0
    done

let replier () =
    Lwt_log.warning_f "Starting replier..." >>
    let sock = Nanomsg.socket_exn Nanomsg.Rep in
    let _ = Nanomsg.bind_exn sock (Nanomsg.Addr.bind_of_string "inproc://test") in
    while%lwt true
    do
        Nanomsg_lwt.recv_string sock >>= fun msg_req ->
        Lwt_log.warning_f "Replier received '%s'." msg_req >>
        let msg_rep = msg_req |> int_of_string |> (+) 1 |> string_of_int in
        Nanomsg_lwt.send_string sock msg_rep >>
        Lwt_log.warning_f "Replier replied '%s' to request '%s'." msg_rep msg_req
    done

let () =
    Lwt_main.run
    begin
        Lwt.async requester;
        Lwt.async replier;
        while%lwt true do Lwt_unix.sleep 60.0 done
    end

