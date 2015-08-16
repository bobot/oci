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
open Oci_Common
open Oci_Std

type artefact_api = {
  binaries : Oci_Filename.t;
  oci_data : Oci_Filename.t;
  oci_simple_exec : Oci_Filename.t;
  superroot: user;
  root: user;
  user: user;
} with sexp, bin_io


let get_configuration = Rpc.Rpc.create
    ~name:"Oci_Artefact.start"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:bin_artefact_api

type exec_in_namespace_response =
  | Exec_Ok
  | Exec_Error of string
with bin_io

let exec_in_namespace = Rpc.Rpc.create
    ~name:"Oci_Monitor.exec_in_namespace"
    ~version:1
    ~bin_query:Oci_Wrapper_Api.bin_parameters
    ~bin_response:bin_exec_in_namespace_response

let start_in_namespace
    ?implementations ~exec_in_namespace ~parameters ~named_pipe () =
  debug "Here";
  let named_pipe_in = named_pipe^".in" in
  let named_pipe_out = named_pipe^".out" in
  unlink_no_fail named_pipe_in
  >>= fun () ->
  unlink_no_fail named_pipe_out
  >>= fun () ->
  Unix.mkfifo named_pipe_in
  >>= fun () ->
  Unix.mkfifo named_pipe_out
  >>= fun () ->
  Unix.chmod ~perm:0o666 named_pipe_in
  >>= fun () ->
  Unix.chmod ~perm:0o666 named_pipe_out
  >>= fun () ->
  debug "Pipe created at %s and %s" named_pipe_in named_pipe_out;
  let conn =
    Writer.open_file named_pipe_in
    >>= fun writer ->
    Reader.open_file named_pipe_out
    >>= fun reader ->
    Unix.unlink named_pipe_in
    >>= fun () ->
    Unix.unlink named_pipe_out
    >>= fun () ->
    Rpc.Connection.create
      ~connection_state:(fun _ -> ())
      ?implementations
      reader
      writer
    >>= fun conn ->
    let conn = Result.ok_exn conn in
    return conn
  in
  let error = exec_in_namespace (parameters:Oci_Wrapper_Api.parameters) in
  return (error,conn)


let rpc_create =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.create"
    ~version:1
    ~bin_query:Oci_Filename.bin_t
    ~bin_response:bin_artefact

type rpc_link_to_query = artefact * Oci_Filename.t with bin_io

let rpc_link_to =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.link_to"
    ~version:1
    ~bin_query:bin_rpc_link_to_query
    ~bin_response:Unit.bin_t

let rpc_copy_to =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.copy_to"
    ~version:1
    ~bin_query:bin_rpc_link_to_query
    ~bin_response:Unit.bin_t

let rpc_stop_runner =
  Rpc.Rpc.create
    ~name:"Oci_Runner.stop_runner"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:Unit.bin_t

