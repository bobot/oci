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

type t

val start:
  implementations:
    Async.Std.Rpc.Connection.t Rpc.Implementation.t list ->
  never_returns
(** The runner waits for request. *)

val implement:
  ('query,'result) Oci_Data.t ->
  (t -> 'query -> 'result Deferred.t) ->
  Async.Std.Rpc.Connection.t Rpc.Implementation.t

type artefact = Oci_Common.Artefact.t with sexp, bin_io

val create_artefact: t -> dir:string -> artefact Deferred.t
val link_artefact:
  t -> ?user:Oci_Common.user_kind
  -> artefact -> dir:string -> unit Deferred.t
(** ro *)
val copy_artefact:
  t -> ?user:Oci_Common.user_kind
  -> artefact -> dir:string -> unit Deferred.t
(** rw *)

val get_internet: t -> unit Deferred.t
val git_clone: t ->
  ?user:Oci_Common.user_kind ->
  url:string ->
  dst:Oci_Filename.t ->
  unit Deferred.t

val dispatch:
  t -> ('query,'result) Oci_Data.t -> 'query -> 'result Or_error.t Deferred.t
val dispatch_exn:
  t -> ('query,'result) Oci_Data.t -> 'query -> 'result Deferred.t


val std_log: t -> ('a, unit, string, unit) format4 -> 'a
val err_log: t -> ('a, unit, string, unit) format4 -> 'a
val cha_log: t -> ('a, unit, string, unit) format4 -> 'a
val cmd_log: t -> ('a, unit, string, unit) format4 -> 'a

val process_log: t -> Process.t -> unit

val process_create:
  t -> Process.t Or_error.t Deferred.t Process.with_create_args
(** both Process.create and process_log *)

val run: t -> unit Deferred.t Process.with_create_args
