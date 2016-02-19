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

type ('query,'result) t

val register:
  name:string ->
  version:int ->
  bin_query:'query Core.Std.Bin_prot.Type_class.t ->
  bin_result:'result Core.Std.Bin_prot.Type_class.t ->
  ('query,'result) t

open Async.Std

exception NoResult


val name: ('query,'result) t -> string
val version: ('query,'result) t -> int
val rpc: ('query,'result) t -> ('query,'result Or_error.t) Rpc.Rpc.t
(** return the first data of the log, or raise {!NoResult} if none *)
val log: ('query,'result) t ->
  ('query, 'result Oci_Log.line, Error.t) Rpc.Pipe_rpc.t
val forget: ('query,'result) t -> ('query, unit Or_error.t) Rpc.Rpc.t
(** If the 'query are memoized forget the previous computation.
    does nothing if there is nothing to forget *)


val id:   ('query,'result) t -> ('query * 'result) Type_equal.Id.t

