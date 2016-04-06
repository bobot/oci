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


val clone:
  user:Oci_Common.user_kind ->
  url:String.t ->
  dst:Oci_Filename.t ->
  commit:Oci_Common.Commit.t ->
  unit Deferred.t

val copy_file:
  user:Oci_Common.user_kind ->
  url:String.t ->
  src:Oci_Filename.t ->
  dst:Oci_Filename.t ->
  commit:Oci_Common.Commit.t ->
  unit Deferred.t

val read_file:
  url:String.t ->
  src:Oci_Filename.t ->
  commit:Oci_Common.Commit.t ->
  string Deferred.t

val merge_base:
  url:String.t ->
  Oci_Common.Commit.t ->
  Oci_Common.Commit.t ->
  Oci_Common.Commit.t Deferred.t

val commit_of_revspec:
  url:String.t ->
  revspec:String.t ->
  Oci_Common.Commit.t option Deferred.t

val commit_of_branch:
  url:String.t ->
  branch:String.t ->
  Oci_Common.Commit.t option Deferred.t

val last_commit_before:
  url:String.t ->
  branch:String.t ->
  time:Time.t ->
  Oci_Common.Commit.t option Deferred.t

val time_of_commit:
  url:String.t ->
  commit:Oci_Common.Commit.t ->
  Time.t Deferred.t

val init:
  dir:string ->
  register_saver:(loader:(unit -> unit Deferred.t) ->
                  saver:(unit -> unit Deferred.t) ->
                  unit) ->
  identity_file:string option ->
  unit
