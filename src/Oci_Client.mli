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
open Oci_Common

module Git: sig
  type formatted_proc

  val formatted_proc : (int -> string, unit, string) format -> formatted_proc

  type env = [ `Extend of (string * string) list
             | `Replace of (string * string) list ]

  type args = [ `Proc of formatted_proc | `S of string ]

  val mk_proc: (int -> string, unit, string) format -> args

  type cmd

  val exec:
    ?args:args list ->
    ?env:env ->
    ?proc_requested:int ->
    ?working_dir:Oci_Filename.filename ->
    string -> cmd

  val run: ?env:env -> string -> string list -> cmd

  val make:
    ?j:int ->
    ?vars:(string * string) list ->
    ?env :env ->
    string list ->
    cmd

  val git_clone:
    url:string ->
    ?dir:Oci_Filename.t ->
    Commit.t ->
    cmd

  val git_copy_file:
    url:string ->
    src:Oci_Filename.t ->
    dst:Oci_Filename.t ->
    Commit.t ->
    cmd

  type repo

  val repo:
    deps:Core.Std.String.t list ->
    cmds:cmd list ->
    tests:cmd list ->
    repo

  type t

  val used_repos: t -> repo String.Map.t
  val filter_deps_for: t -> t
  val invariant: t -> bool
  val repos:
    name:string ->
    repos:repo String.Map.t ->
    rootfs:Oci_Rootfs_Api.Rootfs.t ->
    t

  type connection = Async_extra.Import.Rpc_kernel.Connection.t


  type exec = {
    cmd: string;
    args: [ `S of string | `Proc of formatted_proc ] list;
    env : [ `Replace of (string * string) list
          | `Extend of (string * string) list];
    proc_requested : int;
    working_dir: Oci_Filename.t (** Relative path *)
  }

  type compile_and_tests =
    [ `Artefact of Artefact.t
    | `Cmd of exec * Unix.Exit_or_signal.t * Timed.t
    | `Dependency_error of Core.Std.String.Set.t ]

  val pp: Format.formatter -> compile_and_tests -> unit

  val compile_and_tests:
    connection -> t ->
    (compile_and_tests Oci_Log.line Async_kernel.Std.Pipe.Reader.t)
      Async_kernel.Std.Deferred.t

  type xpra = [compile_and_tests | `XpraDir of Oci_Filename.t ]

  val pp_xpra: Format.formatter -> xpra -> unit

  val xpra:
    connection -> t ->
    (xpra Oci_Log.line Async_kernel.Std.Pipe.Reader.t)
      Async_kernel.Std.Deferred.t

  val merge_base:
    connection ->
    url:string ->
    Commit.t -> Commit.t ->
    Commit.t Deferred.t

  val commit_of_revspec:
    connection ->
    url:string ->
    revspec:string ->
    Commit.t option Deferred.t

  val commit_of_branch:
    connection ->
    url:string ->
    branch:string ->
    Commit.t option Deferred.t

  val last_commit_before:
    connection ->
    url:string ->
    branch:string ->
    time:Time.t ->
    Commit.t option Deferred.t

  val time_of_commit:
    connection ->
    url:string ->
    commit:Commit.t ->
    Time.t Deferred.t

end

module Cmdline: sig

  type repo = {name:string;url:string}

  val mk_repo:
    ?revspec:string ->
    url:string ->
    deps:repo list ->
    cmds:Git.cmd list ->
    ?tests:Git.cmd list ->
    string (** name *) -> repo

  module Predefined: sig
    val ocaml: repo
    val ocamlfind: repo
    val zarith: repo
    val xmllight: repo
    val camlp4: repo
    val lablgtk: repo
    val ocamlgraph: repo
    val ounit: repo
    val cryptokit: repo
  end

  type create_query_hook =
    (query:Oci_Generic_Masters_Api.CompileGitRepo.Query.t ->
     revspecs:string option String.Map.t ->
     Oci_Generic_Masters_Api.CompileGitRepo.Query.t *
     string option Core.Std.String.Map.t)
      Cmdliner.Term.t

  val default_cmdline:
    ?create_query_hook:create_query_hook (* experts only *) ->
    unit -> unit Deferred.t
  (** parse cmdline, run, and quit *)

  (** {2 Experts only} *)
  val git_clone: ?dir:Oci_Filename.t -> string -> Git.cmd
  val add_repo: string -> Git.repo -> unit
  val add_default_revspec_for_url:
    revspec:string -> url:string -> name:string -> unit

end
