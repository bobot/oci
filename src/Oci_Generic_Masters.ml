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

let compile_deps =
  let open Oci_Generic_Masters_Api.CompileGitRepo in
  fun (q:Query.t) ->
    let deps = String.Map.remove (Query.used_repos q) q.name in
    Deferred.List.map (String.Map.keys deps)
      ~how:`Parallel
      ~f:(fun dep_name ->
          let dep = Query.filter_deps_for {q with name = dep_name} in
          Oci_Master.dispatch_master_exn ~msg:dep_name rpc dep)

let compile_git_repo q =
  compile_deps q
  >>= fun results ->
  let repo = String.Map.find_exn q.repos q.name in
  Oci_Master.simple_runner ~binary_name ~error:(fun _ -> raise Exit) begin
    fun conn ->
      Oci_Master.dispatch_runner_exn
        Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc conn {
        rootfs = q.rootfs;
        cmds=repo.cmds;
        artefacts = List.map ~f:(fun r -> r.artefact) results;
      }
  end

let xpra_git_repo q =
  compile_deps q
  >>= fun results ->
  let repo = String.Map.find_exn q.repos q.name in
  Oci_Master.start_runner ~binary_name
  >>= fun (err,conn) ->
  choose [
    choice (err >>= function
      | Oci_Master.Exec_Ok -> never ()
      | Exec_Error s -> return s) (fun _ -> raise Exit);
    choice begin
      conn >>= fun conn ->
      Oci_Master.dispatch_runner_exn
        Oci_Generic_Masters_Api.XpraRunner.rpc conn
        {
          rootfs = q.rootfs;
          cmds = repo.cmds;
          artefacts = List.map ~f:(fun r -> r.artefact) results;
        }
    end (fun x -> x);
  ]

let init_compile_git_repo () =
  MasterCompileGitRepoArtefact.create_master
    Oci_Generic_Masters_Api.CompileGitRepo.rpc
    compile_git_repo;

  (** Xpra *)
  Oci_Master.register Oci_Generic_Masters_Api.XpraGitRepo.rpc
    (Oci_Master.simple_master xpra_git_repo);

  (** RemoteBranch *)
  Oci_Master.register
    Oci_Generic_Masters_Api.GitRemoteBranch.rpc
    (fun q ->
       Monitor.try_with_or_error
         (fun () ->
            Oci_Git.get_remote_branch_commit ~url:q.url ~revspec:q.revspec),
       Oci_Log.null)
