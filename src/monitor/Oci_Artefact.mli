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

type t with sexp, bin_type_class

val create: Oci_Filename.t -> t Deferred.t

val link_to: t -> Oci_Filename.t -> unit Deferred.t
(** ro only *)

val copy_to: t -> Oci_Filename.t -> unit Deferred.t
(** rw *)

val is_available: t -> bool Deferred.t

val remove_dir: Oci_Filename.t -> unit Deferred.t


val register_master:
  ('query,'result) Oci_Data.t ->
  ('query -> 'result Deferred.t) ->
  unit


val run: unit -> never_returns

val start_runner:
  binary_name:string ->
  (Oci_Artefact_Api.exec_in_namespace_response Async_kernel.Deferred0.t *
   Async.Std.Rpc.Connection.t Async_kernel.Deferred0.t)
    Async.Std.Deferred.t
(** Start the given runner in a namespace and start an Rpc connection.
    `start_runner ~binary_name` start the executable
    [binary_name^".native"] located in the directory of binaries *)

val permanent_directory:
  ('query,'result) Oci_Data.t -> Oci_Filename.t Deferred.t
(** Give the permanent directory for this master *)
