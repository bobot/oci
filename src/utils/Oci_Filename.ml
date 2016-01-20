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

include FilePath

(* include FilePath.UnixPath *)

type t = string with sexp, bin_io, compare

let pp = String.pp

let equal = String.equal

(* let t_of_sexp x = filename_of_string (String.t_of_sexp x) *)
(* let sexp_of_t x = String.sexp_of_t (string_of_filename x) *)

(* let bin_size_t x = String.bin_size_t (string_of_filename x) *)
(* let bin_write_t c x = String.bin_write_t c (string_of_filename x) *)

(* let mk = filename_of_string *)
(* let get = string_of_filename *)

let mk x = x
let get x = x

let is_subdir ~parent ~children =
  equal parent children ||  is_subdir children parent
let reparent ~oldd ~newd file = reparent oldd newd file
