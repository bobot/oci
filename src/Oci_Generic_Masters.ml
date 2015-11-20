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

let compile_git_repo conn =
  let open Oci_Generic_Masters_Api.CompileGitRepo in
  fun (q:Query.t) ->
    ignore (Query.used_repos q); (** test well formedness *)
    let repo = String.Map.find_exn q.repos q.name in
    Deferred.List.map repo.deps
      ~how:`Parallel
      ~f:(fun dep_name ->
          let dep = Query.filter_deps_for {q with name = dep_name} in
          Oci_Master.dispatch_master_exn
            ~msg:dep_name rpc dep)
    >>= fun results ->
    Oci_Master.dispatch_runner_exn
      Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc conn
      {
        url=repo.url;
        commit = repo.commit;
        rootfs = q.rootfs;
        cmds=repo.cmds;
        artefacts = List.map ~f:(fun r -> r.artefact) results;
      }

let init_compile_git_repo () =
  MasterCompileGitRepoArtefact.create_master_and_runner
    Oci_Generic_Masters_Api.CompileGitRepo.rpc
    ~error:(fun _ -> raise Exit)
    ~binary_name
    compile_git_repo;

  (** RemoteBranch *)
  Oci_Artefact.register_master
    Oci_Generic_Masters_Api.GitRemoteBranch.rpc
    (fun q ->
       Monitor.try_with_or_error
         (fun () ->
            Oci_Git.get_remote_branch_commit ~url:q.url ~refspec:q.refspec),
       Oci_Log.null)
