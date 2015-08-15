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
open Async.Std

type t = Rpc.Connection.t

(** Enter inside the namespace *)
let () =
  Caml.Unix.chroot ".";
  Caml.Unix.chdir "/"

let run ~implementations =
  begin
    let implementations =
      Rpc.Implementations.create_exn
        ~on_unknown_rpc:`Raise
        ~implementations in
    let named_pipe = Sys.argv.(1) in
    Reader.open_file (named_pipe^".in")
    >>> fun reader ->
    Writer.open_file (named_pipe^".out")
    >>> fun writer ->
    Rpc.Connection.create
      ~implementations
      ~connection_state:(fun c -> c)
      reader writer
    >>> fun conn ->
    let conn = Result.ok_exn conn in
    Shutdown.at_shutdown (fun () ->
        Rpc.Connection.close conn
        >>= fun () ->
        Reader.close reader;
        >>= fun () ->
        Writer.close writer
      )
  end;
  Scheduler.go ()


type artefact = Oci_Common.artefact with sexp
let bin_artefact = Oci_Common.bin_artefact

let create_artefact t ~dir =
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_create t dir
let link_artefact t src ~dir =
    Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_link_to t (src,dir)
let copy_artefact t src ~dir =
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_copy_to t (src,dir)

let dispatch t d q = Rpc.Rpc.dispatch_exn (Oci_Data.rpc d) t q
