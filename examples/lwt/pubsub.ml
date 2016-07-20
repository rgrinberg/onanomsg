open Lwt
open Result

let publisher () =
    Lwt_log.warning_f "Starting publisher..." >>
    let sock = Nanomsg.socket_exn Nanomsg.Pub in
    let _ = Nanomsg.bind_exn sock (Nanomsg.Addr.bind_of_string "inproc://test") in
    while%lwt true
    do
        let msg = Printf.sprintf "%04X" (Random.int 65536) in
        Nanomsg_lwt.send_string sock msg >>
        Lwt_log.warning_f "Publisher sent '%s'." msg >>
        Lwt_unix.sleep 1.0
    done

let subscriber prefix () =
    Lwt_log.warning_f "Starting subscriber '%s'..." prefix >>
    let sock = Nanomsg.socket_exn Nanomsg.Sub in
    begin match Nanomsg.subscribe sock prefix with
        | Ok ()        -> Lwt_log.warning_f "Subscriber '%s' successfully limited subscription..." prefix
        | Error (v, s) -> Lwt_log.error_f "Subscriber '%s' unable to limit subscription: %s, %s" prefix v s
    end >>
    let _ = Nanomsg.connect_exn sock (Nanomsg.Addr.connect_of_string "inproc://test") in
    while%lwt true
    do
        Nanomsg_lwt.recv_string sock >>= fun msg ->
        Lwt_log.warning_f "Subscriber '%s' received '%s'." prefix msg
    done

let () =
    Lwt_main.run
    begin
        Lwt.async publisher;
        Lwt_unix.sleep 1.0 >>= fun () ->
        String.iter (fun prefix -> Lwt.async (subscriber (String.make 1 prefix))) "0123456789ABCDEF";
        while%lwt true do Lwt_unix.sleep 60.0 done
    end

