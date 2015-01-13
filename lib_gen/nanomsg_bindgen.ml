open Ctypes

let _ =
  let fmt = Format.formatter_of_out_channel (open_out "lib/nanomsg_stubs.c") in
  Format.fprintf fmt "#include <caml/mlvalues.h>@.";
  Format.fprintf fmt "#include <nanomsg/nn.h>@.";
  Cstubs.write_c fmt ~prefix:"caml_" (module Nanomsg_bindings.C);

  let fmt = Format.formatter_of_out_channel (open_out "lib/nanomsg_generated.ml") in
  Cstubs.write_ml fmt ~prefix:"caml_" (module Nanomsg_bindings.C)
