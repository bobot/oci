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

(** Manage directory resulting from a task execution *)
open Core.Std
open Async.Std

type t = Oci_Common.Artefact.t

type runner

val create:
  prune:Oci_Filename.t list ->
  rooted_at:Oci_Filename.t ->
  only_new:Bool.t ->
  src:Oci_Filename.t ->
  t Deferred.t

val link_to: Oci_Common.user_kind -> t -> Oci_Filename.t -> unit Deferred.t
(** ro only *)

val copy_to: Oci_Common.user_kind -> t -> Oci_Filename.t -> unit Deferred.t
(** rw *)

val is_available: t -> bool Deferred.t

val remove_dir: Oci_Filename.t -> unit Deferred.t


val register_master:
  ?forget: ('query -> unit Or_error.t Deferred.t) ->
  ('query,'result) Oci_Data.t ->
  ('query -> 'result Oci_Log.reader) ->
  unit

val register_saver:
  name:string ->
  loader:(unit -> unit Deferred.t) ->
  saver:(unit -> unit Deferred.t) ->
  unit

val run: unit -> never_returns

val start_runner:
  debug_info:string ->
  binary_name:string ->
  (unit Or_error.t Deferred.t * runner) Async.Std.Deferred.t
(** Start the given runner in a namespace and start an Rpc connection.
    `start_runner ~binary_name` start the executable
    [binary_name^".native"] located in the directory of binaries *)

val runner_conn: runner -> Async.Std.Rpc.Connection.t Deferred.t option
val runner_conn_or_never: runner -> Async.Std.Rpc.Connection.t Deferred.t

val stop_runner: runner -> unit Deferred.t

val permanent_directory:
  ('query,'result) Oci_Data.t -> Oci_Filename.t Deferred.t
(** Give the permanent directory for this master *)


val dispatch_master:
  ('query,'result) Oci_Data.t ->
  'query -> 'result Or_error.t Deferred.t

val dispatch_master_exn:
  ('query,'result) Oci_Data.t ->
  'query -> 'result Deferred.t


val dispatch_master_log:
  ('query,'result) Oci_Data.t ->
  'query -> 'result Oci_Log.reader
