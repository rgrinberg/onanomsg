open Ctypes
open Foreign

let af_sp           = 1
let af_sp_raw       = 2
let nn_sockaddr_max = 128
let nn_sol_socket   = 0

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

type nn_iovec
let nn_iovec : nn_iovec structure typ = structure "nn_iovec"
let iov_base = nn_iovec *:* (ptr void)
let iov_len = nn_iovec *:* size_t
let () = seal nn_iovec

type nn_msghdr
let nn_msghdr : nn_msghdr structure typ = structure "nn_msghdr"
let msg_iov = nn_msghdr *:* (ptr nn_iovec)
let msg_iovlen = nn_msghdr *:* int
let msg_control = nn_msghdr *:* (ptr void)
let msg_controllen = nn_msghdr *:* size_t
let () = seal nn_msghdr

type nn_cmsghdr
let nn_cmsghdr : nn_cmsghdr structure typ = structure "nn_cmsghdr"
let cmsg_len = nn_cmsghdr *:* size_t
let cmsg_level = nn_cmsghdr *:* int
let cmsg_type = nn_cmsghdr *:* int

let nn_errno = foreign "nn_errno"
    (void @-> returning int)

let nn_strerror = foreign "nn_strerror"
    (int @-> returning string)

let nn_symbol = foreign "nn_symbol"
    (int @-> ptr int @-> returning string)

let nn_term = foreign "nn_term" 
    (void @-> returning void)

let nn_allocmsg = foreign "nn_allocmsg"
    (size_t @-> int @-> returning (ptr void))

let nn_freemsg = foreign "nn_freemsg"
    (ptr void @-> returning int)

let nn_socket = foreign "nn_socket" 
    (int @-> int @-> returning int)

let nn_close = foreign "nn_close" 
    (int @-> returning int)

let nn_setsockopt = foreign "nn_setsockopt" 
    (int @-> int @-> int @-> (ptr void) @-> (ptr size_t) @-> returning int)

let nn_getsockopt = foreign "nn_getsockopt"
    (int @-> int @-> int @-> (ptr void) @-> (ptr size_t) @-> returning int)

let nn_bind = foreign "nn_bind"
    (int @-> string @-> returning int)

let nn_connect = foreign "nn_connect"
    (int @-> string @-> returning int)

let nn_shutdown = foreign "nn_shutdown"
    (int @-> int @-> returning int)

let nn_send = foreign "nn_send"
    (int @-> ptr void @-> size_t @-> int @-> returning int)

let nn_recv = foreign "nn_recv"
    (int @-> ptr void @-> size_t @-> int @-> returning int)

let nn_sendmsg = foreign "nn_sendmsg"
    (int @-> ptr nn_msghdr @-> int @-> returning int)

let nn_recvmsg = foreign "nn_recvmsg"
    (int @-> ptr nn_msghdr @-> int @-> returning int)

let nn_device = foreign "nn_device"
    (int @-> int @-> returning int)
