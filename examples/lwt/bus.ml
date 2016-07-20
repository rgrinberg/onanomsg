open Lwt

let peer self_port ports () =
    Lwt_log.warning_f "starting peer '%d'..." self_port >>
    let sock = Nanomsg.socket_exn Nanomsg.Bus in
    let _ =  Nanomsg.bind_exn sock (`Inproc (string_of_int self_port)) in
    Lwt_unix.sleep 1.0 >>
    let connect port =
        if port <> self_port
        then ignore (Nanomsg.connect_exn sock (`Inproc (string_of_int port))) in
    List.iter connect ports;
	let sender () =
		while%lwt true
		do
			let msg = Printf.sprintf "%4X" (Random.int 65536) in
        	Nanomsg_lwt.send_string sock msg >>
        	Lwt_log.warning_f "Peer '%d' sent '%s'." self_port msg >>
			Lwt_unix.sleep (Random.float 1.0)
		done in
	let receiver () =
		while%lwt true
		do
        	Nanomsg_lwt.recv_string sock >>= fun msg ->
        	Lwt_log.warning_f "Peer '%d' received '%s'." self_port msg
		done in
	sender () <?> receiver ()

let () =
    let ports = [9000; 9001; 9002] in
    Lwt_main.run
    begin
        Lwt.async (peer 9000 ports);
        Lwt.async (peer 9001 ports);
        Lwt.async (peer 9002 ports);
        while%lwt true do Lwt_unix.sleep 60.0 done
    end

