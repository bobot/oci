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

let test_succ = Oci_Data.register
    ~name:"succ"
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_result:Int.bin_t

let test_fibo = Oci_Data.register
    ~name:"fibo"
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_result:Int.bin_t

let test_fibo_artefact_aux = Oci_Data.register
    ~name:"fibo_artefact_aux"
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_result:Oci_Common.Artefact.bin_t

let test_fibo_artefact = Oci_Data.register
    ~name:"fibo_artefact"
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_result:Int.bin_t

let test_fibo_error_artefact = Oci_Data.register
    ~name:"fibo_error_artefact"
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_result:Int.bin_t

module Ocaml_Query = struct

  type t = {
    rootfs: Oci_Rootfs_Api.Rootfs.t;
    commit: Oci_Common.Commit.t;
  } [@@deriving sexp, bin_io, compare]

  let hash = Hashtbl.hash

end

let test_ocaml = Oci_Data.register
    ~name:"ocaml.compilation"
    ~version:1
    ~bin_query:Ocaml_Query.bin_t
    ~bin_result:Oci_Common.Artefact.bin_t
