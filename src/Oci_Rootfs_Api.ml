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

type rootfs_info = {
  distribution: string;
  release: string;
  arch: string;
  packages: string list;
  (** additional packages that have been installed *)
  comment: string;
} with sexp, bin_io

module Rootfs_Id = Int

type rootfs = {
  id: Rootfs_Id.t;
  info: rootfs_info;
  rootfs: Oci_Common.Artefact.t
} with sexp, bin_io


type create_rootfs_query = {
  rootfs_info : rootfs_info;
  rootfs_tar: Oci_Filename.t; (** absolute pathname *)
  meta_tar: Oci_Filename.t option; (** absolute pathname *)
} with sexp, bin_io

let create_rootfs = Oci_Data.register
    ~name:"Oci.Rootfs.create"
    ~version:1
    ~bin_query:bin_create_rootfs_query
    ~bin_result:bin_rootfs

let find_rootfs = Oci_Data.register
    ~name:"Oci.Rootfs.find"
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_result:bin_rootfs
