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

module CompileGitRepoRunner = struct

  type cmd = {
    cmd: string;
    args: [ `S of string |
            `Proc] list;
    env : [ `Replace of (string * string) list
          | `Extend of (string * string) list];
    proc_requested : int;
    kind: [`Required | `Test]
  } with sexp, bin_io, compare
  (** `Proc replaced by number of processus *)

  module Query = struct
    type t = {
      rootfs: Oci_Rootfs_Api.Rootfs.t;
      commit: Oci_Common.Commit.t;
      cmds: cmd list;
      url: string;
      artefacts: Oci_Common.Artefact.t list;
    } with sexp, bin_io, compare

    let hash = Hashtbl.hash
  end

  module Result = struct
    type t = {
      artefact: Oci_Common.Artefact.t;
      failures: string list;
    } with sexp, bin_io, compare
  end

  let rpc =
    Oci_Data.register
      ~name:"Oci_Generic_Masters.compile_git_repo_runner"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_result:Result.bin_t
end

module CompileGitRepo = struct
  module Query = struct
    type t = {
      name: string;
      rootfs: Oci_Rootfs_Api.Rootfs.t;
      commits: Oci_Common.Commit.t String.Map.t;
    } with sexp, bin_io, compare

    let hash = Hashtbl.hash
  end

  module Result = CompileGitRepoRunner.Result

  let rpc =
    Oci_Data.register
      ~name:"Oci_Generic_Masters.compile_git_repo"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_result:Result.bin_t
end

module GitRemoteBranch = struct
  module Query = struct
    type t = {
      name: string;
      refspec: String.t;
    } with sexp, bin_io, compare
  end

  let rpc =
    Oci_Data.register
      ~name:"Oci_Generic_Masters.git_remote_branch"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_result:(Option.bin_t Oci_Common.Commit.bin_t)
end
