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

let oci_at_shutdown = Oci_Artefact_Api.oci_at_shutdown

type runner_result = Oci_Artefact_Api.exec_in_namespace_response =
  | Exec_Ok
  | Exec_Error of string with bin_io

let register data f = Oci_Artefact.register_master data f
let run () = Oci_Artefact.run ()
let start_runner ~binary_name = Oci_Artefact.start_runner ~binary_name
let stop_runner conn =
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_stop_runner conn ()
let permanent_directory = Oci_Artefact.permanent_directory

let create_master ~hashable data f =
  let db : ('query, 'result Deferred.t) Hashtbl.t =
    Hashtbl.create ~hashable () in
  let f q =
    match Hashtbl.find db q with
    | Some r -> r
    | None ->
      let ivar = Ivar.create () in
      let ivar_d = Ivar.read ivar in
      Hashtbl.add_exn db ~key:q ~data:ivar_d;
      begin
        f q
        >>> fun r ->
        Ivar.fill ivar r
      end;
      ivar_d
  in
  register data f

let create_master_and_runner
    ~hashable data ?(binary_name=Oci_Data.name data) ~error f =
  create_master ~hashable data
    begin fun q ->
      start_runner ~binary_name
      >>= fun (err,conn) ->
      choose [
        choice (err >>= function
          | Exec_Ok -> never ()
          | Exec_Error s -> return s) error;
        choice begin
          conn >>= fun conn ->
          Shutdown.at_shutdown (fun () -> Rpc.Connection.close conn);
          f conn q
          >>= fun res ->
          stop_runner conn
          >>= fun () ->
          Rpc.Connection.close conn;
          >>= fun () ->
          return res
        end (fun x -> x);
      ]
    end

