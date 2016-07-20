open Lwt

type role = Binder | Connector

let string_of_role= function
	| Binder    -> "binder"
	| Connector -> "connector"

let peer role () =
    Lwt_log.warning_f "starting peer '%s'..." (string_of_role role) >>
    let sock = Nanomsg.socket_exn Nanomsg.Pair in
	let _ = match role with
		| Binder    -> Nanomsg.bind_exn sock (Nanomsg.Addr.bind_of_string "inproc://test")
    	| Connector -> Nanomsg.connect_exn sock (Nanomsg.Addr.connect_of_string "inproc://test") in
	let sender () =
		while%lwt true
		do
			let msg = Printf.sprintf "%4X" (Random.int 65536) in
        	Nanomsg_lwt.send_string sock msg >>
        	Lwt_log.warning_f "Peer '%s' sent '%s'." (string_of_role role) msg >>
			Lwt_unix.sleep (Random.float 1.0)
		done in
	let receiver () =
		while%lwt true
		do
        	Nanomsg_lwt.recv_string sock >>= fun msg ->
        	Lwt_log.warning_f "Peer '%s' received '%s'." (string_of_role role) msg
		done in
	sender () <?> receiver ()

let () =
    Lwt_main.run
    begin
        Lwt.async (peer Binder);
        Lwt_unix.sleep 1.0 >>= fun () ->
        Lwt.async (peer Connector);
        while%lwt true do Lwt_unix.sleep 60.0 done
    end

