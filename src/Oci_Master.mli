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

(** {2 Simple API} *)

val run: unit -> never_returns
(** Once all the masters have been registered *)

type runner

module Make (Query: Hashtbl.Key_binable) (Result : Binable.S) : sig

  val create_master:
    (Query.t,Result.t) Oci_Data.t ->
    (Query.t -> Result.t Deferred.t) ->
    unit

  val create_master_unit:
    (Query.t,Result.t) Oci_Data.t ->
    (Query.t -> Result.t Oci_Log.writer -> unit Deferred.t) ->
    unit

  val create_master_and_runner:
    (Query.t,Result.t) Oci_Data.t ->
    ?binary_name:string ->
    ?error:(Error.t -> Result.t) ->
    (runner -> Query.t -> Result.t Deferred.t) ->
    unit

  val create_master_and_reusable_runner:
    (Query.t,Result.t) Oci_Data.t ->
    ?binary_name:string ->
    ?error:(Error.t -> Result.t) ->
    hashable_key:'k Hashtbl.Hashable.t ->
    extract_key:(Query.t -> 'k) ->
    ?timeout:Time.Span.t ->
    (first:bool -> runner -> Query.t -> Result.t Deferred.t) ->
    unit

end

val dispatch_runner:
  ?msg:string ->
  ('query,'result) Oci_Data.t ->
  runner ->
  'query -> 'result Or_error.t Deferred.t
val dispatch_runner_exn:
  ?msg:string ->
  ('query,'result) Oci_Data.t ->
  runner ->
  'query -> 'result Deferred.t

val dispatch_runner_log:
  ?msg:string ->
  'result Oci_Log.writer ->
  ('query,'result) Oci_Data.t ->
  runner ->
  'query -> unit Deferred.t

val dispatch_master:
  ?msg:string ->
  ('query,'result) Oci_Data.t ->
  'query -> 'result Or_error.t Deferred.t
val dispatch_master_exn:
  ?msg:string ->
  ('query,'result) Oci_Data.t ->
  'query -> 'result Deferred.t
val dispatch_master_log:
  ?msg:string ->
  ('query,'result) Oci_Data.t ->
  'query -> 'result Oci_Log.reader


val attach_log: 'a Oci_Log.writer -> (unit -> 'b) -> 'b
val std_log: ('a, unit, string, unit) format4 -> 'a
val err_log: ('a, unit, string, unit) format4 -> 'a
val cha_log: ('a, unit, string, unit) format4 -> 'a
val cmd_log: ('a, unit, string, unit) format4 -> 'a

(** {2 Expert API} *)

val oci_at_shutdown: (unit -> unit Deferred.t) -> unit
(** Run when the masters will stop *)

val register:
  ?forget: ('query -> unit Or_error.t Deferred.t) ->
  ('query,'result) Oci_Data.t ->
  ('query -> 'result Oci_Log.reader) ->
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

val simple_runner:
  debug_info:string ->
  binary_name:string ->
  ?error:(Error.t -> 'a) ->
  (runner -> 'a Deferred.t) ->
  'a Deferred.t

val simple_master:
  ('a -> 'b Deferred.t) ->
  'a -> 'b Oci_Log.reader

val simple_master_unit:
  ('a -> 'result Oci_Log.writer -> unit Deferred.t) ->
  'a -> 'result Oci_Log.reader

val register_saver:
  name:string ->
  loader:(unit -> unit Deferred.t) ->
  saver:(unit -> unit Deferred.t) ->
  unit

type slot

val start_runner:
  debug_info:string ->
  binary_name:string ->
  ?slot:slot ->
  unit ->
  (unit Or_error.t Deferred.t * runner) Async.Std.Deferred.t
(** Start the given runner in a namespace and start an Rpc connection.
    `start_runner ~binary_name` start the executable
    [binary_name^".native"] located in the directory of binaries.
    Return a pair of defered the first one is determined in case of
    error of the process, the second one is determined when the
    connection is established.
*)

val reusable_runner:
  hashable_key:'k Hashtbl.Hashable.t ->
  debug_info:('k -> string) ->
  binary_name:('k -> string) ->
  ?timeout:Time.Span.t ->
  ?error:('k -> 'd -> Error.t -> 'a) ->
  (first:bool -> runner -> 'k -> 'd -> 'a Deferred.Or_error.t) ->
  (* partial application *)
  'k -> 'd -> 'a Deferred.Or_error.t


val stop_runner: runner -> unit Deferred.t
(** Ask or force the runner to stop *)

val alloc_slot: unit -> slot Async.Std.Deferred.t

val freeze_runner: runner -> unit Deferred.t
val unfreeze_runner: runner -> slot -> unit Deferred.t

val permanent_directory:
  ('query,'result) Oci_Data.t -> Oci_Filename.t Deferred.t
(** Give the permanent directory for this master *)

val oci_version: string
