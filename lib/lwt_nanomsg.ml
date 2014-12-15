open Lwt

let rec send socket msg =
  try return @@ Onanomsg.send ~block:false socket msg
  with Onanomsg.Error (Onanomsg.E_AGAIN, _, _) ->
    Lwt_unix.yield () >> send socket msg

let rec recv socket =
  try return @@ Onanomsg.recv ~block:false socket
  with
  | Onanomsg.Error ((Onanomsg.E_AGAIN | Onanomsg.E_INVAL), _, _) ->
    Lwt_unix.yield () >> recv socket
