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

type run_query = {
  prog: string;
  args: string list;
  runas: Oci_Common.user_kind;
} with sexp, bin_io

let run = Oci_Data.register
    ~name:"Oci_Cmd_Runner_Api.run"
    ~version:1
    ~bin_query:bin_run_query
    ~bin_result:Unit.bin_t


let create_artefact = Oci_Data.register
    ~name:"Oci_Cmd_Runner.create"
    ~version:1
    ~bin_query:Oci_Filename.bin_t
    ~bin_result:Oci_Common.Artefact.bin_t

type link_copy_query = {
  user: Oci_Common.user_kind;
  artefact: Oci_Common.Artefact.t;
  dst: Oci_Filename.t;
} with sexp, bin_io

let link_to = Oci_Data.register
    ~name:"Oci_Cmd_Runner_Api.link_to"
    ~version:1
    ~bin_query:bin_link_copy_query
    ~bin_result:Unit.bin_t

let copy_to = Oci_Data.register
    ~name:"Oci_Cmd_Runner_Api.copy_to"
    ~version:1
    ~bin_query:bin_link_copy_query
    ~bin_result:Unit.bin_t

let get_internet =
  Oci_Data.register
    ~name:"Oci_Cmd_Runner_Api.get_internet"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_result:Unit.bin_t
