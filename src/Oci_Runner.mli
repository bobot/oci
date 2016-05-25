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

type 'r t

val start:
  implementations:
    Async.Std.Rpc.Connection.t Rpc.Implementation.t list ->
  never_returns
(** The runner waits for request. *)

exception StopQuery
(** Stop the query. It is not an error in itself. The log sent to
    master to here *)

val implement:
  ('query,'result) Oci_Data.t ->
  ('result t -> 'query -> 'result Deferred.t) ->
  Async.Std.Rpc.Connection.t Rpc.Implementation.t

val implement_unit:
  ('query,'result) Oci_Data.t ->
  ('result t -> 'query -> unit Deferred.t) ->
  Async.Std.Rpc.Connection.t Rpc.Implementation.t

type artefact = Oci_Common.Artefact.t [@@deriving sexp, bin_io]

val create_artefact:
  ?rooted_at:Oci_Filename.t ->
  (** default: "/" *)
  ?prune:Oci_Filename.t list ->
  ?only_new:bool ->
  (** specifies if linked files should be forgotten (default: true) *)
  'r t ->
  dir:string -> artefact Deferred.t
val link_artefact:
  'r t -> ?user:Oci_Common.user_kind
  -> artefact -> dir:string -> unit Deferred.t
(** ro *)
val copy_artefact:
  'r t -> ?user:Oci_Common.user_kind
  -> artefact -> dir:string -> unit Deferred.t
(** rw *)

val get_internet: 'r t -> unit Deferred.t
val git_clone: 'r t ->
  ?user:Oci_Common.user_kind ->
  url:string ->
  dst:Oci_Filename.t ->
  commit:Oci_Common.Commit.t ->
  unit Deferred.t
val git_copy_file: 'r t ->
  ?user:Oci_Common.user_kind ->
  url:string ->
  src:Oci_Filename.t ->
  dst:Oci_Filename.t ->
  commit:Oci_Common.Commit.t ->
  unit Deferred.t
val get_file: 'r t ->
  kind:[`MD5] ->
  checksum:string ->
  dst:Oci_Filename.t ->
  unit Deferred.t
val give_external_access: 'r t -> Oci_Filename.t -> Oci_Filename.t Deferred.t

val get_proc: 'r t -> int -> int Deferred.t
val release_proc: 'r t -> int -> unit Deferred.t
val get_release_proc: 'r t -> int -> (int -> 'a Deferred.t) -> 'a Deferred.t


val dispatch:
  'r t -> ('query,'result) Oci_Data.t -> 'query -> 'result Or_error.t Deferred.t
val dispatch_exn:
  'r t -> ('query,'result) Oci_Data.t -> 'query -> 'result Deferred.t


val std_log: 'r t -> ('a, unit, string, unit) format4 -> 'a
val err_log: 'r t -> ('a, unit, string, unit) format4 -> 'a
val cha_log: 'r t -> ('a, unit, string, unit) format4 -> 'a
val cmd_log: 'r t -> ('a, unit, string, unit) format4 -> 'a
val data_log: 'result t -> 'result -> unit

(* val process_log: 'r t -> Process.t -> unit Deferred.t *)

type 'a process_create
  =  ?env         : Process.env  (** default is [`Extend []] *)
  -> ?working_dir : string
  -> prog         : string
  -> args         : string list
  -> unit
  -> 'a Deferred.t

val print_cmd: string -> string list -> string

val run: 'r t ->
  Core.Std.Unix.Exit_or_signal.t process_create

val run_timed: 'r t ->
 ?timelimit:Time.Span.t -> (* soft limit, wall clock *)
 (Core.Std.Unix.Exit_or_signal.t * Oci_Common.Timed.t)
  process_create

exception CommandFailed
val run_exn: 'r t -> unit process_create
(** Same as {!run} but raise CommandFailed in case of error *)

val run_timed_exn:
  'r t -> Oci_Common.Timed.t process_create

val oci_version: string
