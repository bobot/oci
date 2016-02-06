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

type formatted_proc =
  Oci_Generic_Masters_Api.CompileGitRepoRunner.Formatted_proc.t

let mk = Oci_Generic_Masters_Api.CompileGitRepoRunner.Formatted_proc.mk

let exec
    ?(args=[])
    ?(env=`Extend [])
    ?(proc_requested=1)
    ?(working_dir=Oci_Filename.current_dir)
    cmd =
  Oci_Generic_Masters_Api.CompileGitRepoRunner.Exec
    {Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd;
     args;proc_requested;working_dir;env}

type cmd = Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd
type repo = Oci_Generic_Masters_Api.CompileGitRepo.Query.repo

let repo ~deps ~cmds ~tests =
  {Oci_Generic_Masters_Api.CompileGitRepo.Query.deps;cmds;tests}

type t = Oci_Generic_Masters_Api.CompileGitRepo.Query.t
let repos ~name ~repos ~rootfs =
  {Oci_Generic_Masters_Api.CompileGitRepo.Query.name;rootfs;repos}

let used_repos = Oci_Generic_Masters_Api.CompileGitRepo.Query.used_repos
let filter_deps_for =
  Oci_Generic_Masters_Api.CompileGitRepo.Query.filter_deps_for
let invariant = Oci_Generic_Masters_Api.CompileGitRepo.Query.invariant

type compile_and_tests =
  [ `Artefact of Oci_Common.Artefact.t
  | `Cmd of
       Oci_Generic_Masters_Api.CompileGitRepoRunner.exec *
       (unit,
        Oci_Generic_Masters_Api.CompileGitRepoRunner.Result.exit_or_signal)
         Core.Std.Result.t * Oci_Common.Timed.t
  | `Dependency_error of Core.Std.String.Set.t ]

let pp =
  Oci_Generic_Masters_Api.CompileGitRepo.Result.pp

type connection = Async_extra.Import.Rpc_kernel.Connection.t

type exec = Oci_Generic_Masters_Api.CompileGitRepoRunner.exec = {
  cmd: string;
  args: [ `S of string | `Proc of formatted_proc ] list;
  env : [ `Replace of (string * string) list
        | `Extend of (string * string) list];
  proc_requested : int;
  working_dir: Oci_Filename.t (** Relative path *)
}

let compile_and_tests t repos =
  Rpc.Pipe_rpc.dispatch_exn
    (Oci_Data.log Oci_Generic_Masters_Api.CompileGitRepo.rpc)
    t repos
  >>= fun (p,_) ->
  return p

type xpra = [compile_and_tests | `XpraDir of Oci_Filename.t ]

let pp_xpra = Oci_Generic_Masters_Api.XpraGitRepo.Result.pp

let xpra t repos =
  Rpc.Pipe_rpc.dispatch_exn
    (Oci_Data.log Oci_Generic_Masters_Api.XpraGitRepo.rpc) t repos
  >>= fun (p,_) ->
  return p

let merge_base t ~url commit1 commit2 =
  Rpc.Rpc.dispatch_exn
    (Oci_Data.rpc Oci_Generic_Masters_Api.GitMergeBase.rpc)
    t {Oci_Generic_Masters_Api.GitMergeBase.Query.url;commit1;commit2}
  >>= fun r ->
  return (Or_error.ok_exn r)

let commit_of_revspec t ~url ~revspec =
  Rpc.Rpc.dispatch_exn
    (Oci_Data.rpc Oci_Generic_Masters_Api.GitCommitOfRevSpec.rpc)
    t {Oci_Generic_Masters_Api.GitCommitOfRevSpec.Query.url;revspec}
  >>= fun r ->
  return (Or_error.ok_exn r)

let commit_of_branch t ~url ~branch =
  Rpc.Rpc.dispatch_exn
    (Oci_Data.rpc Oci_Generic_Masters_Api.GitCommitOfBranch.rpc)
    t {Oci_Generic_Masters_Api.GitCommitOfBranch.Query.url;branch}
  >>= fun r ->
  return (Or_error.ok_exn r)

let last_commit_before t ~url ~branch ~time =
  Rpc.Rpc.dispatch_exn
    (Oci_Data.rpc Oci_Generic_Masters_Api.GitLastCommitBefore.rpc)
    t {Oci_Generic_Masters_Api.GitLastCommitBefore.Query.url;branch;time}
  >>= fun r ->
  return (Or_error.ok_exn r)

let time_of_commit t ~url ~commit =
  Rpc.Rpc.dispatch_exn
    (Oci_Data.rpc Oci_Generic_Masters_Api.GitTimeOfCommit.rpc)
    t {Oci_Generic_Masters_Api.GitTimeOfCommit.Query.url;commit}
  >>= fun r ->
  return (Or_error.ok_exn r)

