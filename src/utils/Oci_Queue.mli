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

open Async.Std

(** A queue where elements can't be removed. Can be also seen as a
    pipe that return the same elements to all the readers. Keep in
    memory all the elements previously inserted
*)

type 'a t

val create: unit -> 'a t
val read: 'a t -> 'a Pipe.Reader.t
val add: 'a t -> 'a -> unit
(** add an element *)
val transfer_id: 'a t -> 'a Pipe.Reader.t -> unit Deferred.t
(** add all the elements read from the pipe *)
val eof: 'a t -> unit
(** Close the queue, no more elements can be added *)
