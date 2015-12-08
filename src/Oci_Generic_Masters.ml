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
    ~f:(fun _ -> true) (** always the first result *)
  >>= function
  | Some (Core_kernel.Result.Ok
            (`Compilation (`Ok r))) ->
    Oci_Master.cha_log "Dependency %s done" dep_name;
    return (Some r)
          | _ ->
            Oci_Master.err_log
              "Dependency %s failed (or one of its dependency)" dep_name;
            return None


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
    >>= fun artefacts ->
    return (List.map artefacts ~f:(function
        | Some r -> r
        | None -> raise Dependency_error
      ))

let compile_git_repo q log =
  compile_deps q
  >>= fun results ->
  let repo = String.Map.find_exn q.repos q.name in
  Oci_Master.simple_runner ~binary_name ~error:(fun _ -> raise Exit) begin
    fun conn ->
      Oci_Master.dispatch_runner_log log
        Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc conn {
        rootfs = q.rootfs;
        cmds=repo.cmds;
        tests=repo.tests;
        artefacts = results;
      }
  end

let xpra_git_repo q log =
  compile_deps q
  >>= fun results ->
  let repo = String.Map.find_exn q.repos q.name in
  Oci_Master.start_runner ~binary_name
  >>= fun (err,conn) ->
  choose [
    choice (err >>= function
      | Oci_Master.Exec_Ok -> never ()
      | Oci_Master.Exec_Error s -> return s) (fun _ -> raise Exit);
    choice begin
      conn >>= fun conn ->
      Oci_Master.dispatch_runner_log log
        Oci_Generic_Masters_Api.XpraRunner.rpc conn
        {
          rootfs = q.rootfs;
          cmds = repo.cmds;
          tests = repo.tests;
          artefacts = results;
        }
    end (fun x -> x);
  ]

let init_compile_git_repo () =
  MasterCompileGitRepoArtefact.create_master_unit
    Oci_Generic_Masters_Api.CompileGitRepo.rpc
    compile_git_repo;

  (** Xpra *)
  Oci_Master.register Oci_Generic_Masters_Api.XpraGitRepo.rpc
    (Oci_Master.simple_master_unit xpra_git_repo);

  (** RemoteBranch *)
  Oci_Master.register
    Oci_Generic_Masters_Api.GitRemoteBranch.rpc
    (fun q ->
       Oci_Log.init (fun log ->
           Monitor.try_with_or_error (fun () ->
               Oci_Git.get_remote_branch_commit ~url:q.url ~revspec:q.revspec)
           >>= fun res ->
           Oci_Log.add log (Oci_Log.data res)
         ))
