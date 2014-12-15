open Ctypes
open Foreign

let nn_sockaddr_max      = 128

(* socket options *)
let nn_linger            = 1
let nn_sndbuf            = 2
let nn_rcvbuf            = 3
let nn_sndtimeo          = 4
let nn_rcvtimeo          = 5
let nn_reconnect_ivl     = 6
let nn_reconnect_ivl_max = 7
let nn_sndprio           = 8
let nn_sndfd             = 10
let nn_rcvfd             = 11
let nn_domain            = 12
let nn_protocol          = 13
let nn_ipv4only          = 14
(* send/recv options *)
let nn_dontwait          = 1

let nn_msg = Unsigned.Size_t.of_int (-1)

type nn_iovec
let nn_iovec : nn_iovec structure typ = structure "nn_iovec"
let iov_base = field nn_iovec "iov_base" (ptr void)
let iov_len  = field nn_iovec "iov_len" size_t
let () = seal nn_iovec

type nn_msghdr
let nn_msghdr : nn_msghdr structure typ = structure "nn_msghdr"
let msg_iov        = field nn_msghdr "msg_iov" (ptr nn_iovec)
let msg_iovlen     = field nn_msghdr "msg_iovlen" int
let msg_control    = field nn_msghdr "msg_control" (ptr void)
let msg_controllen = field nn_msghdr "msg_controllen" size_t
let () = seal nn_msghdr

type nn_cmsghdr
let nn_cmsghdr : nn_cmsghdr structure typ = structure "nn_cmsghdr"
let cmsg_len   = field nn_cmsghdr "cmsg_len" size_t
let cmsg_level = field nn_cmsghdr "cmsg_level" int
let cmsg_type  = field nn_cmsghdr "cmsg_type" int
let () = seal nn_cmsghdr

type nn_symbol_properties
let nn_symbol_properties : nn_symbol_properties structure typ =
  structure "nn_symbol_properties"
let nnsym_value = field nn_symbol_properties "value" int
let nnsym_name = field nn_symbol_properties "name" string
let nnsym_ns = field nn_symbol_properties "ns" int
let nnsym_type = field nn_symbol_properties "type" int
let nnsym_unit = field nn_symbol_properties "unit" int
let () = seal nn_symbol_properties

let from = Dl.(dlopen ~filename:"libnanomsg.so" ~flags:[RTLD_NOW])

let nn_errno      = foreign ~from "nn_errno" (void @-> returning int)
let nn_strerror   = foreign ~from "nn_strerror" (int @-> returning string)
let nn_term       = foreign ~from "nn_term" (void @-> returning void)
let nn_socket     = foreign ~from "nn_socket" (int @-> int @-> returning int)
let nn_close      = foreign ~from "nn_close" (int @-> returning int)
let nn_bind       = foreign ~from "nn_bind" (int @-> string @-> returning int)
let nn_connect    = foreign ~from "nn_connect" (int @-> string @-> returning int)
let nn_shutdown   = foreign ~from "nn_shutdown" (int @-> int @-> returning int)

(** Message allocation *)

let nn_allocmsg = foreign ~from "nn_allocmsg"
    (size_t @-> int @-> returning (ptr_opt void))
let nn_reallocmsg = foreign ~from "nn_reallocmsg"
    (ptr void @-> size_t @-> returning (ptr_opt void))
let nn_freemsg = foreign ~from "nn_freemsg"
    (ptr void @-> returning int)

(** Send / Recv *)

let nn_send = foreign ~from "nn_send"
    (int @-> ptr (ptr void) @-> size_t @-> int @-> returning int)
let nn_recv = foreign ~from "nn_recv"
    (int @-> ptr (ptr void) @-> size_t @-> int @-> returning int)
let nn_sendmsg = foreign ~from "nn_sendmsg"
    (int @-> ptr nn_msghdr @-> int @-> returning int)
let nn_recvmsg = foreign ~from "nn_recvmsg"
    (int @-> ptr nn_msghdr @-> int @-> returning int)

(** Setsockopt / Getsockopt *)

let nn_getsockopt = foreign ~from "nn_getsockopt"
    (int @-> int @-> int @-> (ptr void) @-> (ptr size_t) @-> returning int)
let nn_setsockopt = foreign ~from "nn_setsockopt"
    (int @-> int @-> int @-> (ptr void) @-> size_t @-> returning int)

let nn_device = foreign ~from "nn_device" (int @-> int @-> returning int)

(** Runtime access to nanomsg's symbols *)

let nn_symbol = foreign ~from "nn_symbol"
    (int @-> ptr int @-> returning string)
let nn_symbol_info = foreign ~from "nn_symbol_info"
    (int @-> ptr nn_symbol_properties @-> int @-> returning int)
