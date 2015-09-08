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

let binary_name = "Oci_Generic_Masters_Runner"

(** Git Repo *)

module MasterCompileGitRepoArtefact =
  Oci_Master.Make
    (Oci_Generic_Masters_Api.CompileGitRepo.Query)
    (Oci_Common.Artefact)

type repo = {
  name : string;
  url : string;
  deps: repo list;
  cmds: Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd list;
}

let run cmd args : Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd = {
  cmd;
  args = List.map args ~f:(fun s -> `S s);
  proc_requested = 1;
}

let make ?(j=1) ?(vars=[]) targets :
  Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd = {
  cmd = "make";
  args =
    `S "-j" :: `Proc ::
    List.map vars ~f:(fun (var,v) -> `S (var^"="^v)) @
    List.map targets ~f:(fun s -> `S s);
  proc_requested = j;
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

let init_compile_git_repo () =
  MasterCompileGitRepoArtefact.create_master_and_runner
    Oci_Generic_Masters_Api.CompileGitRepo.rpc
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
               ~msg:dep.name
               Oci_Generic_Masters_Api.CompileGitRepo.rpc {
               name=dep.name;
               rootfs=q.rootfs;
               commits=filter_deps_for q.commits dep
             })
         >>= fun artefacts ->
         Oci_Master.dispatch_runner_exn
           Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc conn
           {
             url=repo.url;
             commit = String.Map.find_exn q.commits repo.name;
             rootfs = q.rootfs;
             cmds=repo.cmds;
             artefacts;
           }
    )
