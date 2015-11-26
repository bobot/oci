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

module Artefact : sig
  type t with sexp, bin_io, compare
  val to_string: t -> string
  val of_int: int -> t
end

module Commit : sig
  type t with sexp, bin_io, compare
  val invariant: t -> bool
  val to_string: t -> string
  val of_string_exn: string -> t
  val of_string: string -> t option
end

module User: sig
  type t = {uid : int; gid : int} with sexp, compare, bin_io

  val equal: t -> t -> bool
  val pp_t: Format.formatter -> t -> unit
  val pp_chown: t -> string
end

type user_kind =
  | Superroot
  (** A user outside the usernamespace of the runners stronger than the
      root, In Artifact run with superroot as root *)
  | Root
  (** root in the usernamespace of the runners *)
  | User
  (** A simple user in the usernamespace of the runners *)
with sexp, compare, bin_io

val master_user: user_kind -> User.t
val runner_user: user_kind -> User.t
val outside_user: first_user_mapped:User.t ->  user_kind -> User.t
