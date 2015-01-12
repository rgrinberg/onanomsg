#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let lwt = Env.bool "lwt"
let ounit = Env.bool "ounit"

let () =
  Pkg.describe "nanomsg" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "lib/nanomsg";
    Pkg.lib ~cond:lwt ~exts:Exts.module_library "lib/nanomsg_lwt";
    Pkg.bin ~cond:ounit ~auto:true "lib_test/suite";

    Pkg.bin ~auto:true "examples/pipeline";
    Pkg.bin ~auto:true "examples/reqrep";
]
