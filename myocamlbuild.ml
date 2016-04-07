open Ocamlbuild_plugin
open! Command

let () =
 dispatch begin function
 | After_rules ->

    flag ["link"; "library"; "ocaml"; "native"; "use_liboci_stubs"]
         (S[A"-cclib"; A"-loci_stubs"]);

    dep ["link"; "ocaml"; "link_liboci_stubs"] ["src/liboci_stubs.a"]
 | _ -> ()
 end
