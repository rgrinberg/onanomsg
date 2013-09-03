open Ctypes
open Foreign

let af_sp                = 1
let af_sp_raw            = 2

let nn_sockaddr_max      = 128
let nn_sol_socket        = 0

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

let nn_hausnumero        = 156384712
let enotsup              = (nn_hausnumero + 1)
let eprotonosupport      = (nn_hausnumero + 2)
let enobufs              = (nn_hausnumero + 3)
let enetdown             = (nn_hausnumero + 4)
let eaddrinuse           = (nn_hausnumero + 5)
let eaddrnotavail        = (nn_hausnumero + 6)
let econnrefused         = (nn_hausnumero + 7)
let einprogress          = (nn_hausnumero + 8)
let enotsock             = (nn_hausnumero + 9)
let eafnosupport         = (nn_hausnumero + 10)
let eproto               = (nn_hausnumero + 11)
let eagain               = (nn_hausnumero + 12)
let ebadf                = (nn_hausnumero + 13)
let einval               = (nn_hausnumero + 14)
let emfile               = (nn_hausnumero + 15)
let efault               = (nn_hausnumero + 16)
let eaccess              = (nn_hausnumero + 17)
let enetreset            = (nn_hausnumero + 18)
let enetunreach          = (nn_hausnumero + 19)
let ehostunreach         = (nn_hausnumero + 20)
let enotconn             = (nn_hausnumero + 21)
let emsgsize             = (nn_hausnumero + 22)
let etimedout            = (nn_hausnumero + 23)
let econnaborted         = (nn_hausnumero + 24)
let econnreset           = (nn_hausnumero + 25)
let enoprotoopt          = (nn_hausnumero + 26)
let eisconn              = (nn_hausnumero + 27)
let eterm                = (nn_hausnumero + 53)
let efsm                 = (nn_hausnumero + 54)

let nn_proto_pair = 1
let nn_pair = (nn_proto_pair + 16) + 0

type nn_iovec
let nn_iovec : nn_iovec structure typ = structure "nn_iovec"
let iov_base = nn_iovec *:* (ptr void)
let iov_len  = nn_iovec *:* size_t
let () = seal nn_iovec

type nn_msghdr
let nn_msghdr : nn_msghdr structure typ = structure "nn_msghdr"
let msg_iov        = nn_msghdr *:* (ptr nn_iovec)
let msg_iovlen     = nn_msghdr *:* int
let msg_control    = nn_msghdr *:* (ptr void)
let msg_controllen = nn_msghdr *:* size_t
let () = seal nn_msghdr

type nn_cmsghdr
let nn_cmsghdr : nn_cmsghdr structure typ = structure "nn_cmsghdr"
let cmsg_len   = nn_cmsghdr *:* size_t
let cmsg_level = nn_cmsghdr *:* int
let cmsg_type  = nn_cmsghdr *:* int
let () = seal nn_cmsghdr

module Pub_sub = struct
  let nn_proto_pubsub    = 2
  let nn_pub             = nn_proto_pubsub * 16 + 0
  let nn_sub             = nn_proto_pubsub * 16 + 1
  let nn_sub_subscribe   = 1
  let nn_sub_unsubscribe = 2
end

module Req_rep = struct
  let nn_proto_reqrep   = 3
  let nn_req            = nn_proto_reqrep * 16 + 0
  let nn_rep            = nn_proto_reqrep * 16 + 1
  let nn_req_resend_ivl = 1
end

module Survey = struct
  let nn_proto_survey      = 6
  let nn_surveyor          = nn_proto_survey * 16 + 0
  let nn_respondent        = nn_proto_survey * 16 + 1
  let nn_surveyor_deadline = 1
end

module Pipeline = struct
  let nn_proto_pipeline = 5
  let nn_push           = nn_proto_pipeline * 16 + 0
  let nn_pull           = nn_proto_pipeline * 16 + 1
end

module Bus = struct
  let nn_proto_bus = 7
  let nn_bus       = nn_proto_bus * 16 + 0
end

let from = Dl.(dlopen ~filename:"libnanomsg.so" ~flags:[RTLD_NOW])

let nn_errno      = foreign ~from "nn_errno" (void @-> returning int)
let nn_strerror   = foreign ~from "nn_strerror" (int @-> returning string)
let nn_symbol     = foreign ~from "nn_symbol" (int @-> ptr int @-> returning string)
let nn_term       = foreign ~from "nn_term" (void @-> returning void)
let nn_allocmsg   = foreign ~from "nn_allocmsg" (size_t @-> int @-> returning (ptr void))
let nn_freemsg    = foreign ~from "nn_freemsg" (ptr void @-> returning int)
let nn_socket     = foreign ~from "nn_socket" (int @-> int @-> returning int)
let nn_close      = foreign ~from "nn_close" (int @-> returning int)
let nn_setsockopt = foreign ~from "nn_setsockopt" (int @-> int @-> int @-> (ptr void) @-> (ptr size_t) @-> returning int)
let nn_getsockopt = foreign ~from "nn_getsockopt" (int @-> int @-> int @-> (ptr void) @-> (ptr size_t) @-> returning int)
let nn_bind       = foreign ~from "nn_bind" (int @-> string @-> returning int)
let nn_connect    = foreign ~from "nn_connect" (int @-> string @-> returning int)
let nn_shutdown   = foreign ~from "nn_shutdown" (int @-> int @-> returning int)
let nn_send       = foreign ~from "nn_send" (int @-> string @-> size_t @-> int @-> returning int)
let nn_recv       = foreign ~from "nn_recv" (int @-> ptr void @-> size_t @-> int @-> returning int)
let nn_sendmsg    = foreign ~from "nn_sendmsg" (int @-> ptr nn_msghdr @-> int @-> returning int)
let nn_recvmsg    = foreign ~from "nn_recvmsg" (int @-> ptr nn_msghdr @-> int @-> returning int)
let nn_device     = foreign ~from "nn_device" (int @-> int @-> returning int)
