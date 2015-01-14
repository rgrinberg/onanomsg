(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin;;

dispatch
  (MyOCamlbuildBase.dispatch_combine [
    begin function
    | After_rules ->
      rule "cstubs: lib/x_bindings.ml -> x_stubs.c, x_stubs.ml"
        ~prods:["lib/%_stubs.c"; "lib/%_generated.ml"]
        ~deps: ["lib_gen/%_bindgen.byte"]
        (fun env build ->
          Cmd (A(env "lib_gen/%_bindgen.byte")));
      copy_rule "cstubs: lib_gen/x_bindings.ml -> lib/x_bindings.ml"
        "lib_gen/%_bindings.ml" "lib/%_bindings.ml"
    | _ -> ()
    end;
    dispatch_default
  ])
