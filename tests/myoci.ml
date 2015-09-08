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
open Async.Std

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

(** Generic *)

type repo = {
  name : string;
  url : string;
  deps: repo list;
  cmds: (string * string list) list;
}

let repos = String.Table.create ()
let mk_repo ~name ~url ~deps ~cmds =
  let r = {name;url;deps;cmds} in
  String.Table.add_exn repos ~key:name ~data:r;
  r
let rec used_repos s repo =
  List.fold repo.deps
    ~f:used_repos ~init:(String.Set.add s repo.name)

let rec check_deps_for deps repo =
  if not (String.Map.mem deps repo.name)
  then invalid_argf "Missing commit number for %s" repo.name ();
  List.iter ~f:(check_deps_for deps) repo.deps

let filter_deps_for deps repo =
  let used = used_repos String.Set.empty repo in
  String.Map.filter deps ~f:(fun ~key ~data:_ -> String.Set.mem used key)

let () =
  let ocaml = mk_repo
    ~name:"ocaml"
    ~url:"git@git.frama-c.com:bobot/ocaml.git"
    ~deps:[]
    ~cmds:[
      "./configure",[];
      "make",["world.opt"];
      "make",["install"];
    ]
  in
  let ocamlfind = mk_repo
    ~name:"ocamlfind"
    ~url:"git@git.frama-c.com:bobot/ocamlfind.git"
    ~deps:[ocaml]
    ~cmds:[
      "./configure",[];
      "make",["all"];
      "make",["opt"];
      "make",["install"];
    ]
  in
  let ocamlgraph = mk_repo
    ~name:"ocamlgraph"
    ~url:"https://github.com/backtracking/ocamlgraph.git"
    ~deps:[ocaml;ocamlfind]
    ~cmds:[
      "autoconf",[];
      "./configure",[];
      "make",[];
      "make",["install"];
    ]
  in
  let framac = mk_repo
      ~name:"frama-c"
      ~url:"git@git.frama-c.com:frama-c/frama-c.git"
      ~deps:[ocaml;ocamlfind;ocamlgraph]
      ~cmds:[
        "autoconf",[];
        "./configure",[];
        "make",[];
        "make",["install"];
      ]
  in
  let _genassigns = mk_repo
      ~name:"genassigns"
      ~url:"git@git.frama-c.com:frama-c/genassigns.git"
      ~deps:[ocaml;ocamlfind;ocamlgraph;framac]
      ~cmds:[
        "autoconf",[];
        "./configure",[];
        "make",[];
        "make",["install"];
      ]
  in
  ()

module MasterCompileGitRepoArtefact =
  Oci_Master.Make(Tests.CompileGitRepo.Query)(Oci_Common.Artefact)

let () =
  MasterCompileGitRepoArtefact.create_master_and_runner
    Tests.CompileGitRepo.rpc
    ~error:(fun _ -> raise Exit)
    ~binary_name
    (fun conn q ->
       let repo = String.Table.find_exn repos q.name in
       check_deps_for q.commits repo;
       Deferred.List.map
         repo.deps
         ~how:`Parallel
         ~f:(fun dep ->
             Oci_Master.dispatch_master_exn
               Tests.CompileGitRepo.rpc {
               name=dep.name;
               rootfs=q.rootfs;
               commits=filter_deps_for q.commits dep
             })
         >>= fun artefacts ->
         Oci_Master.dispatch_runner_exn Tests.CompileGitRepoRunner.rpc conn
           {
             url=repo.url;
             commit = String.Map.find_exn q.commits repo.name;
             rootfs = q.rootfs;
             cmds=repo.cmds;
             artefacts;
           }
    )


let () = Oci_Rootfs.register_rootfs ()

let () = never_returns (Oci_Master.run ())
