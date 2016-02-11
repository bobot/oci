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
} [@@deriving sexp, bin_io]

module Rootfs_Id = Int

module Rootfs = struct
  module T = struct
    type t = {
      id: Rootfs_Id.t;
      info: rootfs_info;
      rootfs: Oci_Common.Artefact.t;
    } [@@deriving sexp, bin_io]

    let compare x y = Rootfs_Id.compare x.id y.id
    let hash x = Rootfs_Id.hash x.id
  end
  module Hash = Hashable.Make(T)
  include T
  include Hash
end

type create_rootfs_query = {
  rootfs_info : rootfs_info;
  rootfs_tar: Oci_Filename.t; (** absolute pathname *)
  meta_tar: Oci_Filename.t option; (** absolute pathname *)
} [@@deriving sexp, bin_io]

let create_rootfs = Oci_Data.register
    ~name:"Oci.Rootfs.create"
    ~version:1
    ~bin_query:bin_create_rootfs_query
    ~bin_result:Rootfs.bin_t

let find_rootfs = Oci_Data.register
    ~name:"Oci.Rootfs.find"
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_result:Rootfs.bin_t


type add_packages_query = {
  id: Rootfs_Id.t;
  packages: string list;
} [@@deriving sexp, bin_io]

let add_packages =
  Oci_Data.register
    ~name:"Oci.Rootfs.add_packages"
    ~version:1
    ~bin_query:bin_add_packages_query
    ~bin_result:Rootfs.bin_t
