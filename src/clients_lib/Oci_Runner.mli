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

val run:
  implementations: t Rpc.Implementation.t list ->
  never_returns
(** The runner waits for request. *)

type artefact with sexp, bin_type_class

val create_artefact: t -> dir:string -> artefact Deferred.t
val link_artefact: t -> artefact -> dir:string -> unit Deferred.t
(** ro *)
val copy_artefact: t -> artefact -> dir:string -> unit Deferred.t
(** rw *)
val dispatch: t -> ('query,'result) Oci_Data.t -> 'query -> 'result Deferred.t
