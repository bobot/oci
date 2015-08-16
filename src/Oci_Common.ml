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

type artefact = Int.t with sexp, compare, bin_io
let bin_artefact = Int.bin_t

type user = {uid : int; gid : int} with sexp, compare, bin_io

let pp_user fmt u = Format.fprintf fmt "%i,%i" u.uid u.gid
let pp_chmod u = Printf.sprintf "%i:%i" u.uid u.gid

(** user in different namespace *)

type user_kind =
  | Superroot
  (** A user outside the usernamespace of the runners stronger than the
      root, In Artifact run with superroot as root *)
  | Root
  | User

let master_user = function
  | Superroot -> {uid=0;gid=0}
  | Root -> {uid=1;gid=1}
  | User -> {uid=1001;gid=1001}

let runner_user = function
  | Superroot -> invalid_arg "No superroot in runner namespace"
  | Root -> {uid=0;gid=0}
  | User -> {uid=1000;gid=1000}

let outside_user ~first_user_mapped = function
  | Superroot -> {uid=first_user_mapped.uid; gid=first_user_mapped.gid}
  | Root -> {uid=first_user_mapped.uid+1; gid=first_user_mapped.gid+1}
  | User -> {uid=first_user_mapped.uid+1001;gid=first_user_mapped.gid+1001}
