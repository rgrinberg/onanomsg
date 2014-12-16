#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "onanomsg" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "lib/onanomsg";
    Pkg.bin ~auto:true "lib_test/suite";

    Pkg.bin ~auto:true "examples/pipeline";
    Pkg.bin ~auto:true "examples/reqrep";
]
