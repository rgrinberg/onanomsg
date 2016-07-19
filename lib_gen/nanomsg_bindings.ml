open Ctypes

module C(F: Cstubs.FOREIGN) = struct
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

  let nn_errno      = F.(foreign "nn_errno" (void @-> returning int))
  let nn_strerror   = F.(foreign "nn_strerror" (int @-> returning string))
  let nn_term       = F.(foreign "nn_term" (void @-> returning void))
  let nn_device = F.(foreign "nn_device" (int @-> int @-> returning int))

  let nn_socket     = F.(foreign "nn_socket" (int @-> int @-> returning int))
  let nn_close      = F.(foreign "nn_close" (int @-> returning int))
  let nn_bind       = F.(foreign "nn_bind" (int @-> string @-> returning int))
  let nn_connect    = F.(foreign "nn_connect" (int @-> string @-> returning int))
  let nn_shutdown   = F.(foreign "nn_shutdown" (int @-> int @-> returning int))

  (** Message allocation *)

  let nn_allocmsg = F.(foreign "nn_allocmsg"
      (size_t @-> int @-> returning (ptr_opt void)))
  let nn_reallocmsg = F.(foreign "nn_reallocmsg"
      (ptr void @-> size_t @-> returning (ptr_opt void)))
  let nn_freemsg = F.(foreign "nn_freemsg"
      (ptr void @-> returning int))

  (** Send / Recv *)

  let nn_send = F.(foreign "nn_send"
      (int @-> ptr (ptr void) @-> size_t @-> int @-> returning int))
  let nn_recv = F.(foreign "nn_recv"
      (int @-> ptr (ptr void) @-> size_t @-> int @-> returning int))
  let nn_sendmsg = F.(foreign "nn_sendmsg"
      (int @-> ptr nn_msghdr @-> int @-> returning int))
  let nn_recvmsg = F.(foreign "nn_recvmsg"
      (int @-> ptr nn_msghdr @-> int @-> returning int))

  (** Setsockopt / Getsockopt *)

  let nn_getsockopt = F.(foreign "nn_getsockopt"
      (int @-> int @-> int @-> (ptr void) @-> (ptr size_t) @-> returning int))
  let nn_setsockopt = F.(foreign "nn_setsockopt"
      (int @-> int @-> int @-> (ptr void) @-> size_t @-> returning int))

  (** Runtime access to nanomsg's symbols *)

  let nn_symbol = F.(foreign "nn_symbol"
      (int @-> ptr int @-> returning string))
  let nn_symbol_info = F.(foreign "nn_symbol_info"
      (int @-> ptr nn_symbol_properties @-> int @-> returning int))
end
