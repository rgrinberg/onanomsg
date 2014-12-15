#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "onanomsg" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "lib/onanomsg";
    (* Pkg.lib ~exts:Exts.module_library "lib/lwt_nanomsg"; *)
    Pkg.bin ~auto:true "lib_test/test_endpoint";
    Pkg.bin ~auto:true "lib_test/test_opts";
    Pkg.bin ~auto:true "lib_test/test_pubsub";
    Pkg.bin ~auto:true "lib_test/test_reqrep";

    Pkg.bin ~auto:true "examples/pair";
    Pkg.bin ~auto:true "examples/pipeline";
    Pkg.bin ~auto:true "examples/reqrep";
]
