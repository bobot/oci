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

exception Dependency_error

let run_dependency dep dep_name =
  let open Oci_Generic_Masters_Api.CompileGitRepo in
  let r = Oci_Master.dispatch_master_log ~msg:dep_name rpc dep in
  Oci_Log.reader_get_first r
    ~f:(function
        | (Core_kernel.Result.Ok (`Artefact _)) -> true
        | (Core_kernel.Result.Ok (`Dependency_error _)) -> true
        | _ -> false ) (** always the first result *)
  >>= function
  | Some (Core_kernel.Result.Ok (`Artefact artefact)) ->
    Oci_Master.cha_log "Dependency %s done" dep_name;
    return (`Artefact artefact)
  | Some (Core_kernel.Result.Ok (`Dependency_error s)) ->
    Oci_Master.err_log
      "Some dependencies of %s failed" dep_name;
    return (`Dependency_error s)
  | _ ->
    Oci_Master.err_log
      "Dependency %s failed" dep_name;
    return (`Dependency_error (String.Set.singleton dep_name))


let compile_deps =
  let open Oci_Generic_Masters_Api.CompileGitRepo in
  fun (q:Query.t) ->
    let deps = String.Map.remove (Query.used_repos q) q.name in
    Deferred.List.map (String.Map.keys deps)
      ~how:`Parallel
      ~f:(fun dep_name ->
          let dep = Query.filter_deps_for {q with name = dep_name} in
          run_dependency dep dep_name
        )
    (** The order of the application of the artefact is not relevant *)
    >>= fun artefacts ->
    let result =
      List.fold artefacts ~init:(`Artefact []) ~f:(fun acc x ->
          match x, acc with
          | `Artefact x, `Artefact acc -> `Artefact (x::acc)
          | `Dependency_error x, `Artefact _ -> `Dependency_error x
          | `Dependency_error x, `Dependency_error acc ->
            `Dependency_error (String.Set.union x acc)
          | `Artefact _, `Dependency_error _ -> acc)
    in
    return result

let run_git_repo rpc map_type q log =
  compile_deps q
  >>= function
  | `Artefact results ->
    let repo = String.Map.find_exn q.repos q.name in
    Oci_Master.simple_runner ~binary_name ~error:(fun _ -> raise Exit) begin
      fun conn ->
        let log' = Pipe.init (fun log' ->
            Oci_Master.dispatch_runner_log log' rpc conn {
              Oci_Generic_Masters_Api.CompileGitRepoRunner.Query.rootfs =
                q.rootfs;
              cmds=repo.cmds;
              tests=repo.tests;
              save_artefact = repo.save_artefact;
              artefacts = results;
            })
        in
        Pipe.transfer log' log
          ~f:(Oci_Log.map_line map_type)
    end
  | `Dependency_error s ->
    Pipe.write log (Oci_Log.data (Or_error.return (`Dependency_error s)))

let compile_git_repo q log =
  run_git_repo Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc
    (fun (x : Oci_Generic_Masters_Api.CompileGitRepoRunner.Result.t) ->
       (x :> Oci_Generic_Masters_Api.CompileGitRepo.Result.t))
    q log

let xpra_git_repo q log =
  run_git_repo Oci_Generic_Masters_Api.XpraRunner.rpc
    (fun (x : Oci_Generic_Masters_Api.XpraRunner.Result.t) ->
       (x :> Oci_Generic_Masters_Api.XpraGitRepo.Result.t))
    q log

let init_compile_git_repo () =
  MasterCompileGitRepoArtefact.create_master_unit
    Oci_Generic_Masters_Api.CompileGitRepo.rpc
    compile_git_repo;

  (** Xpra *)
  Oci_Master.register Oci_Generic_Masters_Api.XpraGitRepo.rpc
    (Oci_Master.simple_master_unit xpra_git_repo);

  let register_simple_rpc rpc f=
    Oci_Master.register rpc
      (fun q ->
         Oci_Log.init (fun log ->
             Monitor.try_with_or_error (fun () -> f q)
             >>= fun res ->
             Oci_Log.add log (Oci_Log.data res)
           ))
  in

  (** Commit of revspec *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitCommitOfRevSpec.rpc
    (fun q ->
       Oci_Git.commit_of_revspec ~url:q.url ~revspec:q.revspec);

  (** Commit of branch *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitCommitOfBranch.rpc
    (fun q ->
       Oci_Git.commit_of_branch ~url:q.url ~branch:q.branch);

  (** merge base *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitMergeBase.rpc
    (fun q ->
       Oci_Git.merge_base ~url:q.url q.commit1 q.commit1);

  (** last commit before *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitLastCommitBefore.rpc
    (fun q ->
       Oci_Git.last_commit_before ~url:q.url ~branch:q.branch
         ~time:q.time);

  (** time of commit *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitTimeOfCommit.rpc
    (fun q ->
       Oci_Git.time_of_commit ~url:q.url ~commit:q.commit);

  (** download_file *)
  register_simple_rpc
    Oci_Generic_Masters_Api.WgetDownloadFile.rpc
    (fun q ->
       Oci_Wget.download_file ~url:q.url ~kind:q.kind
         ~checksum:q.checksum
    )

