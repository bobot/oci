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

module Rootfs_Id: sig
  type t
  include Interfaces.Intable with type t := t
  include Interfaces.Stringable with type t := t
  include Interfaces.Comparable with type t := t
  include Interfaces.Hashable with type t := t
  include Interfaces.Sexpable with type t := t
  include Interfaces.Binable with type t := t
end

module Rootfs : sig
  type t = {
    id: Rootfs_Id.t;
    info: rootfs_info;
    rootfs: Oci_Common.Artefact.t;
  } [@@deriving sexp, bin_io]

  include Hashable.S with type t := t
end

type create_rootfs_query = {
  rootfs_info : rootfs_info;
  rootfs_tar: Oci_Filename.t; (** absolute pathname *)
  meta_tar: Oci_Filename.t option; (** absolute pathname *)
} [@@deriving sexp, bin_io]

val create_rootfs: (create_rootfs_query,Rootfs.t) Oci_Data.t
val find_rootfs: (Rootfs_Id.t,Rootfs.t) Oci_Data.t

type add_packages_query = {
  id: Rootfs_Id.t;
  packages: string list;
} [@@deriving sexp, bin_io]

val add_packages: (add_packages_query,Rootfs.t) Oci_Data.t
