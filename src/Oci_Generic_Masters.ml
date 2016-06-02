(**************************************************************************)
(*                                                                        *)
(*  This file is part of OCI.                                             *)
(*                                                                        *)
(*  Copyright (C) 2015-2016                                               *)
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
        | `Artefact _ -> true
        | `Dependency_error _ -> true
        | _ -> false ) (* always the first result *)
  >>= function
  | `Found (`Artefact artefact) ->
    Oci_Master.cha_log "Dependency %s done" dep_name;
    return (`Artefact artefact)
  | `Found (`Dependency_error s) ->
    Oci_Master.err_log
      "Some dependencies of %s failed" dep_name;
    return (`Dependency_error s)
  | `Incomplete ->
    Oci_Master.err_log
      "Log of dependency %s is incomplete" dep_name;
    return (`Error (Error.of_string "incomplete dependency"))
  | `Error _ ->
    Oci_Master.err_log
      "Anomaly for the dependency %s" dep_name;
    return (`Error (Error.of_string "anomaly in a dependency"))
  | `NotFound | `Found _ ->
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
    (* The order of the application of the artefact is not relevant *)
    >>= fun artefacts ->
    let result =
      List.fold artefacts ~init:(`Artefact []) ~f:(fun acc x ->
          match x, acc with
          | _, `Error _ -> acc
          | `Artefact x, `Artefact acc -> `Artefact (x::acc)
          | `Dependency_error x, `Artefact _ -> `Dependency_error x
          | `Dependency_error x, `Dependency_error acc ->
            `Dependency_error (String.Set.union x acc)
          | `Artefact _, `Dependency_error _ -> acc
          | `Error err, _ -> `Error err
        )
    in
    return result

let exec_runner map_type rpc log runner todo =
  let log' = Pipe.init (fun log' ->
      Oci_Master.dispatch_runner_log log' rpc runner todo)
  in
  Pipe.transfer log' log
    ~f:(Oci_Log.map_line map_type)

let artefact_list_hashable =
  {Hashtbl.Hashable.sexp_of_t =
     [%sexp_of: (string * Oci_Common.Artefact.t list)];
   hash = (fun (s,al) -> 5 * String.hash s +
                         List.fold_left al ~init:65535
                           ~f:(fun acc e ->
                               7*(Oci_Common.Artefact.hash e) + 9 * acc));
   compare = [%compare: (string * Oci_Common.Artefact.t list)];
  }


let run_runner =
  let reusable = Oci_Master.reusable_runner
      ~debug_info:(fun (name,_) -> sprintf "Run Repo %s" name)
      ~hashable_key:artefact_list_hashable
      ~binary_name:(fun _ -> binary_name)
      (fun ~first runner _
        (log,(todo:Oci_Generic_Masters_Api.CompileGitRepoRunner.Query.t)) ->
        let todo = if first then todo else {todo with artefacts = []} in
        let r,log = Oci_Queue.spy_writer log in
        let r = Oci_Log.get_end r in
        exec_runner
          (fun (x : Oci_Generic_Masters_Api.CompileGitRepoRunner.Result.t) ->
             (x :> Oci_Generic_Masters_Api.CompileGitRepo.Result.t))
          Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc
          log runner todo
        >>= fun () ->
        r
      )
  in
  fun name log todo ->
    match (todo:Oci_Generic_Masters_Api.CompileGitRepoRunner.Query.t) with
    | { save_artefact = false (* todo precise the criteria *) } ->
      reusable (name,todo.Oci_Generic_Masters_Api.
                       CompileGitRepoRunner.Query.artefacts)
        (log,todo)
      >>= fun _ ->
      Deferred.unit
    | _ ->
    Oci_Master.simple_runner
      ~debug_info:(sprintf "Run Repo %s" name)
      ~binary_name
      (fun runner -> exec_runner
          (fun (x : Oci_Generic_Masters_Api.CompileGitRepoRunner.Result.t) ->
             (x :> Oci_Generic_Masters_Api.CompileGitRepo.Result.t))
          Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc
          log
          runner todo)

let xpra_runner name log todo =
  Oci_Master.simple_runner
    ~debug_info:(sprintf "Xpra Repo %s" name)
    ~binary_name
    (fun runner ->
       exec_runner
         (fun (x : Oci_Generic_Masters_Api.XpraRunner.Result.t) ->
            (x :> Oci_Generic_Masters_Api.XpraGitRepo.Result.t))
         Oci_Generic_Masters_Api.XpraRunner.rpc
         log
         runner todo)

let run_git_repo q log get_runner =
  compile_deps q
  >>= function
  | `Artefact results ->
    let repo = String.Map.find_exn q.repos q.name in
    let todo = {
      Oci_Generic_Masters_Api.CompileGitRepoRunner.Query.cmds=repo.cmds;
      tests=repo.tests;
      save_artefact = repo.save_artefact;
      artefacts = q.rootfs.rootfs::results;
    } in
    get_runner q.name log todo
  | `Dependency_error s ->
    Pipe.write log (Oci_Log.data (`Dependency_error s))
  | `Error err -> Error.raise err

let compile_git_repo q log =
  run_git_repo q log run_runner

let xpra_git_repo q log =
  run_git_repo q log xpra_runner

let init_compile_git_repo () =
  MasterCompileGitRepoArtefact.create_master_unit
    Oci_Generic_Masters_Api.CompileGitRepo.rpc
    compile_git_repo;

  (* Xpra *)
  Oci_Master.register Oci_Generic_Masters_Api.XpraGitRepo.rpc
    (Oci_Master.simple_master_unit xpra_git_repo);

  let register_simple_rpc rpc f=
    Oci_Master.register rpc
      (fun q ->
         Oci_Log.init_writer (fun log ->
             Monitor.try_with_or_error ~here:[%here] (fun () -> f q)
             >>= fun res ->
             Oci_Log.write_and_close log res
           ))
  in

  (* Commit of revspec *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitCommitOfRevSpec.rpc
    (fun q ->
       Oci_Git.commit_of_revspec ~url:q.url ~revspec:q.revspec);

  (* Commit of branch *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitCommitOfBranch.rpc
    (fun q ->
       Oci_Git.commit_of_branch ~url:q.url ~branch:q.branch);

  (* merge base *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitMergeBase.rpc
    (fun q ->
       Oci_Git.merge_base ~url:q.url q.commit1 q.commit1);

  (* last commit before *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitLastCommitBefore.rpc
    (fun q ->
       Oci_Git.last_commit_before ~url:q.url ~branch:q.branch
         ~time:q.time);

  (* time of commit *)
  register_simple_rpc
    Oci_Generic_Masters_Api.GitTimeOfCommit.rpc
    (fun q ->
       Oci_Git.time_of_commit ~url:q.url ~commit:q.commit);

  (* download_file *)
  register_simple_rpc
    Oci_Generic_Masters_Api.WgetDownloadFile.rpc
    (fun q ->
       Oci_Wget.download_file ~url:q.url ~kind:q.kind
         ~checksum:q.checksum
    )

