open Onanomsg

val send : socket -> string -> unit Lwt.t
val recv : socket -> string Lwt.t
