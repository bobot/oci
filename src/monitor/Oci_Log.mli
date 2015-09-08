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

type kind =
  | Standard | Error | Chapter | Command
    with sexp, bin_io

type line = {
  kind : kind;
  line : string;
  time : Time.t;
} with sexp, bin_io

val line: kind -> string -> line

type t

include Binable.S with type t := t

val create: unit -> t

exception Closed_Log

val transfer: t -> line Pipe.Reader.t -> unit Deferred.t
val write_without_pushback: t -> line -> unit
val close: t -> unit Deferred.t

val read: t -> line Pipe.Reader.t Deferred.t

val init:
  dir:string ->
  register_saver:(loader:(unit -> unit Deferred.t) ->
                  saver:(unit -> unit Deferred.t) ->
                  unit)
  -> unit

val t_type_id: t Univ_map.Key.t
