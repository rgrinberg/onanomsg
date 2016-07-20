open Lwt

let puller () =
    Lwt_log.warning_f "Starting puller..." >>
    let sock = Nanomsg.socket_exn Nanomsg.Pull in
    let _ = Nanomsg.bind_exn sock (Nanomsg.Addr.bind_of_string "inproc://test") in
    while%lwt true
    do
        Nanomsg_lwt.recv_string sock >>= fun msg ->
        Lwt_log.warning_f "Puller received '%s'." msg
    done

let pusher () =
    Lwt_log.warning_f "Starting pusher..." >>
    let sock = Nanomsg.socket_exn Nanomsg.Push in
    let _ = Nanomsg.connect_exn sock (Nanomsg.Addr.connect_of_string "inproc://test") in
    while%lwt true
    do
        let msg = Printf.sprintf "%4X" (Random.int 65536) in
        Nanomsg_lwt.send_string sock msg >>
        Lwt_log.warning_f "Pusher sent '%s'." msg >>
        Lwt_unix.sleep 1.0
    done

let () =
    Lwt_main.run
    begin
        Lwt.async puller;
        Lwt_unix.sleep 1.0 >>= fun () ->
        Lwt.async pusher;
        while%lwt true do Lwt_unix.sleep 60.0 done
    end

