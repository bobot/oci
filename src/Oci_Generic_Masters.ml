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

(** {2 Git Repo} *)

module MasterCompileGitRepoArtefact =
  Oci_Master.Make
    (Oci_Generic_Masters_Api.CompileGitRepo.Query)
    (Oci_Generic_Masters_Api.CompileGitRepo.Result)

type repo = {
  name : string;
  url : string;
  deps: repo list;
  cmds: Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd list;
}

let run ?(kind=`Required) ?(env=`Extend []) cmd args
  : Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd = {
  cmd;
  args = List.map args ~f:(fun s -> `S s);
  env;
  proc_requested = 1;
  kind;
}

let make ?(j=1) ?(vars=[]) ?(kind=`Required) ?(env=`Extend []) targets :
  Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd = {
  cmd = "make";
  args =
    `S "-j" :: `Proc ::
    List.map vars ~f:(fun (var,v) -> `S (var^"="^v)) @
    List.map targets ~f:(fun s -> `S s);
  env;
  proc_requested = j;
  kind;
}


let repos_db = String.Table.create ()
let mk_repo ~name ~url ~deps ~cmds =
  let r = {name;url;deps;cmds} in
  String.Table.add_exn repos_db ~key:name ~data:r;
  r
let rec used_repos s repo =
  List.fold repo.deps
    ~f:used_repos ~init:(String.Set.add s repo.name)

let check_deps_for deps used_repos =
  String.Set.iter ~f:(fun repo ->
      if not (String.Map.mem deps repo)
      then invalid_argf "Missing commit number for %s" repo ();
    ) used_repos

let filter_deps_for deps used_repos =
  String.Map.filter deps ~f:(fun ~key ~data:_ -> String.Set.mem used_repos key)

let init_compile_git_repo () =
  (** Oci_Generic_Masters_Api.CompileGitRepo *)
  MasterCompileGitRepoArtefact.create_master_and_runner
    Oci_Generic_Masters_Api.CompileGitRepo.rpc
    ~error:(fun _ -> raise Exit)
    ~binary_name
    (fun conn q ->
       let repo = String.Table.find_exn repos_db q.name in
       let repos = used_repos String.Set.empty repo in
       check_deps_for q.commits repos;
       Deferred.List.map
         (String.Set.to_list (String.Set.remove repos repo.name))
         ~how:`Parallel
         ~f:(fun dep ->
             let repo = String.Table.find_exn repos_db dep in
             let repos = used_repos String.Set.empty repo in
             Oci_Master.dispatch_master_exn
               ~msg:dep
               Oci_Generic_Masters_Api.CompileGitRepo.rpc {
               name=dep;
               rootfs=q.rootfs;
               commits=filter_deps_for q.commits repos
             })
         >>= fun results ->
         Oci_Master.dispatch_runner_exn
           Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc conn
           {
             url=repo.url;
             commit = String.Map.find_exn q.commits repo.name;
             rootfs = q.rootfs;
             cmds=repo.cmds;
             artefacts = List.map ~f:(fun r -> r.artefact) results;
           }
    );
  (** RemoteBranch *)
  Oci_Artefact.register_master
    Oci_Generic_Masters_Api.GitRemoteBranch.rpc
    (fun q ->
       let repo = String.Table.find_exn repos_db q.name in
       Monitor.try_with_or_error
         (fun () ->
            Oci_Git.get_remote_branch_commit ~url:repo.url ~refspec:q.refspec),
       Oci_Log.null)
