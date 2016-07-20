open Lwt

let surveyor () =
    Lwt_log.warning_f "Starting surveyor..." >>
    let sock = Nanomsg.socket_exn Nanomsg.Surveyor in
    let _ = Nanomsg.bind_exn sock (Nanomsg.Addr.bind_of_string "inproc://test") in
    while%lwt true
    do
        Lwt_unix.sleep 1.0 >>
        let msg_sur = string_of_int (Random.int 10) in
        Nanomsg_lwt.send_string sock msg_sur >>
        Lwt_log.warning_f "Surveyor starting new survey: sent '%s'." msg_sur >>
        let rec loop () =
            try%lwt
                Nanomsg_lwt.recv_string sock >>= fun msg_res ->
                Lwt_log.warning_f "Surveyor received '%s'." msg_res >>
                loop ()
            with _ ->
                Lwt_log.warning_f "Surveyor finished!" in
        loop ()
    done

let respondent prefix () =
    Lwt_log.warning_f "Starting respondent '%s'..." prefix >>
    let sock = Nanomsg.socket_exn Nanomsg.Respondent in
    let _ = Nanomsg.connect_exn sock (Nanomsg.Addr.connect_of_string "inproc://test") in
    while%lwt true
    do
        Nanomsg_lwt.recv_string sock >>= fun msg_sur ->
        Lwt_log.warning_f "Respondent '%s' received '%s'." prefix msg_sur >>
        let msg_res = prefix ^ msg_sur in
        Nanomsg_lwt.send_string sock msg_res >>
        Lwt_log.warning_f "Respondent '%s' replied '%s'." prefix msg_res
    done

let () =
    Lwt_main.run
    begin
        Lwt.async surveyor;
        String.iter (fun prefix -> Lwt.async (respondent (String.make 1 prefix))) "ABCD";
        while%lwt true do Lwt_unix.sleep 60.0 done
    end

