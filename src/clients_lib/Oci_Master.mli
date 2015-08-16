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

val create_master:
  hashable:('query Hashtbl.Hashable.t) ->
  ('query,'result) Oci_Data.t ->
  ('query -> 'result Deferred.t) ->
  unit

val create_master_and_runner:
  hashable:('query Hashtbl.Hashable.t) ->
  ('query,'result) Oci_Data.t ->
  ?binary_name:string ->
  error:(string -> 'result) ->
  (Rpc.Connection.t -> 'query -> 'result Deferred.t) ->
  unit

val run: unit -> never_returns
(** Once all the masters have been registered *)

(** {2 Expert API} *)

val register:
  ('query,'result) Oci_Data.t ->
  ('query -> 'result Deferred.t) ->
  unit
(** There is only one master of a given sort by session. It must keep
      track of which tasks are running, and which tasks have been
      already run. *)

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
