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
open Oci_Std

let version = 8

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

  let pp_exec fmt e =
    Format.fprintf fmt
      "@[<hv>%s @[%a@]@ %a%a@]"
      e.cmd
      (Pp.print_list Pp.space (fun fmt -> function
           | `S s -> Format.fprintf fmt "[%s]" s
           | `Proc p -> Format.fprintf fmt "[%a]" Formatted_proc.pp p)
      ) e.args
      (fun fmt ->
         let print_env = Pp.print_list Pp.semi
             (Pp.print_pair_delim Pp.nothing Pp.equal Pp.nothing
                String.pp String.pp)
         in
         function
         | `Extend [] -> ()
         | `Replace l ->
           Format.fprintf fmt "@[Replace env: @[%a@]@]@ " print_env l
         | `Extend l ->
           Format.fprintf fmt "@[Extend env: @[%a@]@]@ " print_env l
      ) e.env
      (fun fmt f ->
         if not (Oci_Filename.equal f Oci_Filename.current_dir) then
           Format.fprintf fmt "@[working_dir: @[%a@]@]" Oci_Filename.pp f
      ) e.working_dir

  type gitclone = {
    directory: Oci_Filename.t; (** Relative path *)
    url: string;
    commit: Oci_Common.Commit.t;
  } with sexp, bin_io, compare


  type gitcopyfile = {
    src: Oci_Filename.t; (** Relative path in git repository *)
    dst: Oci_Filename.t; (** Relative path *)
    url: string;
    commit: Oci_Common.Commit.t;
  } with sexp, bin_io, compare

  type cmd =
    | Exec of exec
    | GitClone of gitclone
    | GitCopyFile of gitcopyfile
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
      | `Cmd of exec * [ `Ok of Oci_Common.Timed.t | `Failed ]
      | `Artefact of Oci_Common.Artefact.t
    ]
    with sexp, bin_io, compare

    let pp fmt : t -> unit = function
      | `Artefact artefact ->
        Format.fprintf fmt "New artefact %a created"
          Oci_Common.Artefact.pp artefact
      | `Cmd (_,`Ok time) ->
        Format.fprintf fmt "Ok in %a" Oci_Common.Timed.pp time
      | `Cmd(cmd,`Failed) ->
        Format.fprintf fmt "@[Failed: %a@]" pp_exec cmd
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

    let pp fmt = function
      | #CompileGitRepoRunner.Result.t as x ->
        CompileGitRepoRunner.Result.pp fmt x
      | `XpraDir x -> Format.fprintf fmt "xpra-dir %a" Oci_Filename.pp x
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

  module Result = struct
    type t = [
      | CompileGitRepoRunner.Result.t
      | `Dependency_error of String.Set.t
    ]
    with sexp, bin_io, compare

    let pp fmt = function
      | #CompileGitRepoRunner.Result.t as x ->
        CompileGitRepoRunner.Result.pp fmt x
      | `Dependency_error s ->
        Format.fprintf fmt "The following dependencies failed: %a"
          (Pp.print_iter1 (fun f x -> String.Set.iter ~f x) Pp.semi String.pp)
          s
  end

  let rpc =
    Oci_Data.register
      ~name:"Oci_Generic_Masters.compile_git_repo"
      ~version
      ~bin_query:Query.bin_t
      ~bin_result:Result.bin_t
end

module XpraGitRepo = struct

  module Result = struct
    type t = [
      | XpraRunner.Result.t
      | `Dependency_error of String.Set.t
    ]
    with sexp, bin_io, compare

    let pp fmt = function
      | #XpraRunner.Result.t as x ->
        XpraRunner.Result.pp fmt x
      | `Dependency_error s ->
        Format.fprintf fmt "xpra-dir %a"
          (Pp.print_iter1 (fun f x -> String.Set.iter ~f x) Pp.semi String.pp)
          s
  end

  let rpc =
    Oci_Data.register
      ~name:"XpraGitRepo.compile_git_repo"
      ~version
      ~bin_query:CompileGitRepo.Query.bin_t
      ~bin_result:Result.bin_t

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
