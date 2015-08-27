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

(** {2 Simple API} *)

val run: unit -> never_returns
(** Once all the masters have been registered *)

module Make (Query: Hashtbl.Key_binable) (Result : Binable.S) : sig

  val create_master:
    (Query.t,Result.t) Oci_Data.t ->
    (Query.t -> Result.t Deferred.t) ->
    unit

  val create_master_and_runner:
    (Query.t,Result.t) Oci_Data.t ->
    ?binary_name:string ->
    error:(string -> Result.t) ->
    (Rpc.Connection.t -> Query.t -> Result.t Deferred.t) ->
    unit

end

val dispatch:
  ('query,'result) Oci_Data.t -> Rpc.Connection.t ->
  'query -> 'result Or_error.t Deferred.t
val dispatch_exn:
  ('query,'result) Oci_Data.t -> Rpc.Connection.t ->
  'query -> 'result Deferred.t

(** {2 Expert API} *)

val oci_at_shutdown: (unit -> unit Deferred.t) -> unit
(** Run when the masters will stop *)

val register:
  ('query,'result) Oci_Data.t ->
  ('query -> 'result Or_error.t Deferred.t) ->
  unit
(** There is only one master of a given sort by session. It must keep
      track of which tasks are running, and which tasks have been
      already run. *)

val simple_register_saver:
  ?init:(unit -> unit Deferred.t) ->
  basename:string ->
  loader:('data_to_save -> unit Deferred.t) ->
  saver:(unit -> 'data_to_save Deferred.t) ->
  ('query, 'result) Oci_Data.t ->
  'data_to_save Bin_prot.Type_class.t ->
   unit

val register_saver:
  loader:(unit -> unit Deferred.t) ->
  saver:(unit -> unit Deferred.t) ->
  unit

type runner_result =
  | Exec_Ok
  | Exec_Error of string with bin_io

val start_runner:
  binary_name:string ->
  (runner_result Async_kernel.Deferred0.t *
   Async.Std.Rpc.Connection.t Async_kernel.Deferred0.t)
    Async.Std.Deferred.t
(** Start the given runner in a namespace and start an Rpc connection.
    `start_runner ~binary_name` start the executable
    [binary_name^".native"] located in the directory of binaries.
    Return a pair of defered the first one is determined in case of
    error of the process, the second one is determined when the
    connection is established.
*)

val stop_runner: Rpc.Connection.t -> unit Deferred.t
(** Ask the runner to stop *)

val permanent_directory:
  ('query,'result) Oci_Data.t -> Oci_Filename.t Deferred.t
(** Give the permanent directory for this master *)
