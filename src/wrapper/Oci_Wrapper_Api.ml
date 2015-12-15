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

type idmap = {
  extern_id: Oci_Common.User.t;
  intern_id: Oci_Common.User.t;
  length_id: int;
} with sexp, bin_io

let idmaps ~in_user ~first_user_mapped =
  List.map ~f:(fun (u,length_id) -> {
        extern_id = Oci_Common.outside_user ~first_user_mapped u;
        intern_id = in_user u;
        length_id;
      })

type parameters = {
  rootfs: Oci_Filename.t;
  idmaps: idmap list;
  command: string;
  argv: string list;
  env: (string * string) list;
  runuid: Int.t;
  rungid: Int.t;
  bind_system_mount: bool;
  (** proc, dev, run *)
  prepare_network: bool;
  workdir: Oci_Filename.t option;
  cgroup: string option;
  (** move to the given cgroup *)
} with sexp, bin_io
