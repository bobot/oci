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

module Artefact = struct
  type t = Int.t with sexp, compare, bin_io
  (* let bin_t = Int.bin_t *)
  let to_string = Int.to_string
  let pp = Int.pp
  let empty = -1 (** special value *)


  let of_int_exn i = if i < 0 then invalid_arg "Artefact.of_int" else i

end

module Commit = struct
  exception BadGitCommitFormat of string with sexp
  type t = String.t with sexp, compare, bin_io
  let invariant x =
    String.length x = 40 &&
    String.for_all ~f:(function
        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
        | '0' | '1' | '2' | '3' | '4' | '5'
        | '6' | '7' | '8' | '9' -> true
        | _ -> false) x

  let to_string = Fn.id
  let of_string_exn x =
    if invariant x then x else raise (BadGitCommitFormat x)
  let of_string x =
    if invariant x then Some x else None
end

module User = struct
  type t = {uid : int; gid : int} with sexp, compare, bin_io

  let equal a b = a.uid = b.uid && a.gid = b.gid
  let pp_t fmt u = Format.fprintf fmt "%i,%i" u.uid u.gid
  let pp_chown u = Printf.sprintf "%i:%i" u.uid u.gid
end
open User
(** user in different namespace *)

type user_kind =
  | Superroot
  (** A user outside the usernamespace of the runners stronger than the
      root, In Artifact run with superroot as root *)
  | Root
  | User
with sexp, compare, bin_io

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

module Formatted (X:sig
    type 'a arg
    val template: (string arg, unit, string) format
  end) = struct
  type t = string
  with sexp, bin_io, compare

  let mk x = string_of_format x
  let get x = Scanf.format_from_string x X.template
  let pp = String.pp
end

module Timed = struct
  type t = {
    cpu_kernel: Time.Span.t; (* S *)
    cpu_user:   Time.Span.t; (* U *)
    wall_clock: Time.Span.t; (* e *)
  } with sexp, bin_io, compare

  let pp fmt e =
    Format.fprintf fmt "{kernel:%a; user:%a; wall:%a}"
      Time.Span.pp e.cpu_kernel
      Time.Span.pp e.cpu_user
      Time.Span.pp e.wall_clock

end
