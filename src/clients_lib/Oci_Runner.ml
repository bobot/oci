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

let run ty f =
  begin
    let implementations =
      Rpc.Implementations.create_exn
        ~on_unknown_rpc:`Raise
        ~implementations:[Rpc.Rpc.implement
                            (Oci_Data.rpc ty)
                            (fun _ q -> f q)
                         ] in
    Tcp.connect (Tcp.to_file "/oci/oci.socket")
    >>> fun (_,reader,writer) ->
    Rpc.Connection.create
      ~implementations
      ~connection_state:(fun _ -> ())
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

let create_artefact ~dir = assert false
let link_artefact src ~dir = assert false
let copy_artefact src ~dir = assert false
