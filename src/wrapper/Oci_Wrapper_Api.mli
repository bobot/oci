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
  extern_id: Int.t;
  intern_id: Int.t;
  length_id: Int.t;
} with sexp, bin_io

type parameters = {
  rootfs: Oci_Filename.t;
  uidmap: idmap list;
  gidmap: idmap list;
  command: string;
  argv: string list;
  env: (string * string) list;
  uid: Int.t;
  gid: Int.t;
  bind_system_mount: bool;
  (** proc, dev, run *)
} with sexp, bin_io
