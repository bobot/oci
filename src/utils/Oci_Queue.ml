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

(** Queue on which reader can wait *)

(** I was not able to find something like that in Core/Async
    Async_stream seems deprecated and doesn't keep previous value
 *)

open Core.Std
open Async.Std

type 'a queue =
  | Eof
  | Next of 'a queue Deferred.t * 'a

type 'a t = {
  first: 'a queue Deferred.t;
  mutable next: 'a queue Ivar.t;
}

let create () =
  let first = Ivar.create () in
  {
    first = Ivar.read first;
    next = first;
  }

let read t =
  Pipe.init (fun writer ->
      let rec get q =
        q
        >>= function
        | Eof -> return ()
        | Next (q,v) ->
        Pipe.write writer v
        >>= fun () ->
        get q
      in
      get t.first
    )

let add t v =
  assert (Ivar.is_empty t.next); (** otherwise used after eof *)
  let next = t.next in
  let new_next = Ivar.create () in
  t.next <- new_next;
  Ivar.fill next (Next(Ivar.read new_next,v))

let rec transfer_id t r =
  Pipe.read' r
  >>= function
  | `Eof -> return ()
  | `Ok q ->
    assert (Ivar.is_empty t.next); (** otherwise used after eof *)
    let new_next = Queue.fold
        ~f:(fun next v ->
            let new_next = Ivar.create () in
            let new_last = Next(Ivar.read new_next,v) in
            Ivar.fill next new_last;
            new_next
          ) ~init:t.next q in
    t.next <- new_next;
    transfer_id t r


let eof t =
  Ivar.fill t.next Eof
