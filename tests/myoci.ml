(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Core.Std

let binary_name = "tests_runner"

module MasterInt = Oci_Master.Make(Int)(Int)

let () =
  MasterInt.create_master_and_runner
    Tests.test_succ
    ~error:(fun _ -> Int.min_value)
    ~binary_name
    (Oci_Master.dispatch_runner_exn Tests.test_succ)


let () =
  MasterInt.create_master_and_runner
    Tests.test_fibo
    ~error:(fun _ -> Int.min_value)
    ~binary_name
    (Oci_Master.dispatch_runner_exn Tests.test_fibo)

let () =
  MasterInt.create_master_and_runner
    Tests.test_fibo_artefact
    ~error:(fun _ -> Int.min_value)
    ~binary_name
    (Oci_Master.dispatch_runner_exn Tests.test_fibo_artefact)

let () =
  MasterInt.create_master_and_runner
    Tests.test_fibo_error_artefact
    ~error:(fun _ -> Int.min_value)
    ~binary_name
    (Oci_Master.dispatch_runner_exn Tests.test_fibo_error_artefact)

module MasterIntArtefact = Oci_Master.Make(Int)(Oci_Common.Artefact)

let () =
  MasterIntArtefact.create_master_and_runner
    Tests.test_fibo_artefact_aux
    ~error:(fun _ -> raise Exit)
    ~binary_name
    (Oci_Master.dispatch_runner_exn Tests.test_fibo_artefact_aux)

module MasterOcamlArtefact =
  Oci_Master.Make(Tests.Ocaml_Query)(Oci_Common.Artefact)

let () =
  MasterOcamlArtefact.create_master_and_runner
    Tests.test_ocaml
    ~error:(fun _ -> raise Exit)
    ~binary_name
    (Oci_Master.dispatch_runner_exn Tests.test_ocaml)

let () =
  let open Oci_Generic_Masters in
  let ocaml = mk_repo
    ~name:"ocaml"
    ~url:"git@git.frama-c.com:bobot/ocaml.git"
    ~deps:[]
    ~cmds:[
      run "./configure" [];
      make ["world.opt"];
      make ["install"];
    ]
  in
  let ocamlfind = mk_repo
    ~name:"ocamlfind"
    ~url:"git@git.frama-c.com:bobot/ocamlfind.git"
    ~deps:[ocaml]
    ~cmds:[
      run "./configure" [];
      make ["all"];
      make ["opt"];
      make ["install"];
    ]
  in
  let ocamlgraph = mk_repo
    ~name:"ocamlgraph"
    ~url:"https://github.com/backtracking/ocamlgraph.git"
    ~deps:[ocaml;ocamlfind]
    ~cmds:[
      run "autoconf" [];
      run "./configure" [];
      make [];
      make ["install-findlib"];
    ]
  in
  let zarith = mk_repo
    ~name:"ZArith"
    ~url:"git@git.frama-c.com:bobot/zarith.git"
    ~deps:[ocaml;ocamlfind]
    ~cmds:[
      run "./configure" [];
      make [];
      make ~env:(`Extend ["OCAMLFIND_LDCONF","ignore"]) ["install"];
    ]
  in
  let framac = mk_repo
      ~name:"frama-c"
      ~url:"git@git.frama-c.com:frama-c/frama-c.git"
      ~deps:[ocaml;ocamlfind;ocamlgraph;zarith]
      ~cmds:[
        run "autoconf" [];
        run "./configure" [];
        make ~j:8 [];
        make ["install"];
        make ~j:1 ~kind:`Test ~vars:["PTESTS_OPTS","-error-code -j 8"] ["tests"]
      ]
  in
  let mk_framac_plugin_repo ~name ~url ~deps ~has_tests =
    let compilation = [
      run "autoconf" [];
      run "./configure" [];
      make [];
      make ["install"];
    ]
    in
    let tests =
      if has_tests
      then [make ~kind:`Test ~vars:["PTESTS_OPTS","-error-code -j 4"] ["tests"]]
      else []
    in
    mk_repo
      ~name
      ~url
      ~deps:(framac::deps)
      ~cmds:(compilation@tests)
  in
  let _genassigns = mk_framac_plugin_repo
      ~name:"Genassigns"
      ~url:"git@git.frama-c.com:frama-c/genassigns.git"
      ~deps:[]
      ~has_tests:true
  in
  let eacsl = mk_framac_plugin_repo
      ~name:"E-ACSL"
      ~url:"git@git.frama-c.com:frama-c/e-acsl.git"
      ~deps:[]
      ~has_tests:true
  in
  let _context_from_precondition = mk_framac_plugin_repo
      ~name:"context-from-precondition"
      ~url:"git@git.frama-c.com:signoles/context-from-precondition.git"
      ~deps:[eacsl]
      ~has_tests:false
  in
  let _a3export = mk_framac_plugin_repo
      ~name:"a3export"
      ~url:"git@git.frama-c.com:frama-c/a3export.git"
      ~deps:[]
      ~has_tests:false
  in
  let _mthread = mk_framac_plugin_repo
      ~name:"Mthread"
      ~url:"git@git.frama-c.com:frama-c/mthread.git"
      ~deps:[]
      ~has_tests:true
  in
  let _pathcrawler = mk_framac_plugin_repo
      ~name:"PathCrawler"
      ~url:"git@git.frama-c.com:frama-c/mthread.git"
      ~deps:[]
      ~has_tests:true
  in
  ()


let () =
  Oci_Rootfs.init ();
  Oci_Generic_Masters.init_compile_git_repo ()

let () = never_returns (Oci_Master.run ())
