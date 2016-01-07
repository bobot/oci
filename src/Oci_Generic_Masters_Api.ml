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

let version = 7

module CompileGitRepoRunner = struct

  module Formatted_proc = Oci_Common.Formatted(struct
      type 'a arg = int -> 'a
      let template = format_of_string "%i"
    end)

  type exec = {
    cmd: string;
    args: [ `S of string |
            `Proc of Formatted_proc.t
          ] list;
    env : [ `Replace of (string * string) list
          | `Extend of (string * string) list];
    proc_requested : int;
    working_dir: Oci_Filename.t (** Relative path *)
  } with sexp, bin_io, compare
  (** `Proc replaced by number of processus *)

  type gitclone = {
    directory: Oci_Filename.t; (** Relative path *)
    url: string;
    commit: Oci_Common.Commit.t;
  } with sexp, bin_io, compare


  type gitshowfile = {
    src: Oci_Filename.t; (** Relative path in git repository *)
    dst: Oci_Filename.t; (** Relative path *)
    url: string;
    commit: Oci_Common.Commit.t;
  } with sexp, bin_io, compare

  type cmd =
    | Exec of exec
    | GitClone of gitclone
    | GitShowFile of gitshowfile
  with sexp, bin_io, compare

  type cmds = cmd list
  with sexp, bin_io, compare

  module Query = struct
    type t = {
      rootfs: Oci_Rootfs_Api.Rootfs.t;
      cmds: cmds;
      tests: cmds;
      artefacts: Oci_Common.Artefact.t list;
    } with sexp, bin_io, compare

    let hash = Hashtbl.hash
  end

  module Result = struct
    type t = [
      | `Compilation of [`Ok of Oci_Common.Artefact.t * Oci_Common.Timed.t |
                         `Failed of string]
      | `Test of [ `Ok of Oci_Common.Timed.t | `Failed ] * string
    ]
    with sexp, bin_io, compare
  end

  let rpc =
    Oci_Data.register
      ~name:"Oci_Generic_Masters.compile_git_repo_runner"
      ~version
      ~bin_query:Query.bin_t
      ~bin_result:Result.bin_t
end


module XpraRunner = struct

  module Result = struct
    type t = [
      | CompileGitRepoRunner.Result.t
      | `XpraDir of Oci_Filename.t
    ]
    with sexp, bin_io, compare
  end

  let rpc =
    Oci_Data.register
      ~name:"Oci_Generic_Masters.xpra_runner"
      ~version
      ~bin_query:CompileGitRepoRunner.Query.bin_t
      ~bin_result:Result.bin_t
end

module CompileGitRepo = struct
  (** Compile and tests git repository but
      return after compilation
  *)

  module Query = struct
    type repo = {
      deps: String.t list;
      cmds: CompileGitRepoRunner.cmds;
      tests: CompileGitRepoRunner.cmds;
    } with sexp, bin_io, compare

    type t = {
      name: string;
      rootfs: Oci_Rootfs_Api.Rootfs.t;
      repos: repo String.Map.t;
    } with sexp, bin_io, compare

    exception MissingRepo of string
    let used_repos t =
      let rec aux m name =
        if String.Map.mem m name then m
        else
          match String.Map.find t.repos name with
          | None -> raise (MissingRepo name)
          | Some repo ->
            List.fold repo.deps
              ~init:(String.Map.add m ~key:name ~data:repo)
              ~f:aux
      in
      aux String.Map.empty t.name

    (** Keep only the needed functions, raise MissingRepo if needed *)
    let filter_deps_for t =
      let used_repos = used_repos t in
      {t with repos =
                String.Map.filter t.repos
                  ~f:(fun ~key ~data:_ -> String.Map.mem used_repos key)}

    let invariant t =
      try ignore (used_repos t); true
      with MissingRepo _ -> false

    let hash = Hashtbl.hash
  end

  module Result = CompileGitRepoRunner.Result

  let rpc =
    Oci_Data.register
      ~name:"Oci_Generic_Masters.compile_git_repo"
      ~version
      ~bin_query:Query.bin_t
      ~bin_result:Result.bin_t
end

module XpraGitRepo = struct

  let rpc =
    Oci_Data.register
      ~name:"XpraGitRepo.compile_git_repo"
      ~version
      ~bin_query:CompileGitRepo.Query.bin_t
      ~bin_result:XpraRunner.Result.bin_t

end

module GitRemoteBranch = struct
  module Query = struct
    type t = {
      url : string;
      revspec: String.t;
    } with sexp, bin_io, compare
  end

  let rpc =
    Oci_Data.register
      ~name:"Oci_Generic_Masters.git_remote_branch"
      ~version:2
      ~bin_query:Query.bin_t
      ~bin_result:(Option.bin_t Oci_Common.Commit.bin_t)
end
