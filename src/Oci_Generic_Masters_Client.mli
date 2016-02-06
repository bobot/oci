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

type formatted_proc

val formatted_proc : (int -> string, unit, string) format -> formatted_proc

type cmd

val exec:
  ?args:[ `Proc of formatted_proc | `S of string ] list ->
  ?env:[ `Extend of (string * string) list
       | `Replace of (string * string) list ] ->
  ?proc_requested:int ->
  ?working_dir:Oci_Filename.filename ->
  string -> cmd

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

type repo = Oci_Generic_Masters_Api.CompileGitRepo.Query.repo

val repo:
  deps:Core.Std.String.t list ->
  cmds:cmd list ->
  tests:cmd list ->
  repo

type t = Oci_Generic_Masters_Api.CompileGitRepo.Query.t

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

