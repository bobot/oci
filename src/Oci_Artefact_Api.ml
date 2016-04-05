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

(** master to monitor *)

type artefact_api = {
  binaries : Oci_Filename.t list;
  oci_data : Oci_Filename.t;
  oci_simple_exec : Oci_Filename.t;
  first_user_mapped: User.t;
  debug_level : Bool.t;
  cleanup_rootfs: Bool.t;
  identity_file: string option;
  proc: Int.t List.t List.t;
  master_proc: Int.t List.t;
} [@@deriving sexp, bin_io]

let get_configuration = Rpc.Rpc.create
    ~name:"Oci_Artefact.start"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:bin_artefact_api

let rpc_kill_runner = Rpc.Rpc.create
    ~name:"Oci_Monitor.kill_runner"
    ~version:1
    ~bin_query:Int.bin_t (* runner_id *)
    ~bin_response:Unit.bin_t

let exec_in_namespace = Rpc.Rpc.create
    ~name:"Oci_Monitor.exec_in_namespace"
    ~version:1
    ~bin_query:Oci_Wrapper_Api.bin_parameters
    ~bin_response:(Or_error.bin_t Unit.bin_t)

let heartbeat_config =
  Rpc.Connection.Heartbeat_config.create
    ~timeout:(Time_ns.Span.create ~min:10 ())
    ~send_every:(Time_ns.Span.create ~sec:10 ())

let start_in_namespace
    ?implementations ~initial_state
    ~exec_in_namespace ~parameters ~named_pipe () =
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
    let description =
      Info.create ~here:[%here]
        (sprintf "runner %i" parameters.Oci_Wrapper_Api.runner_id)
        parameters Oci_Wrapper_Api.sexp_of_parameters
    in
    Rpc.Connection.create
      ~connection_state:(fun _ -> initial_state)
      ~description
      ~heartbeat_config:heartbeat_config
      ?implementations
      reader
      writer
    >>= fun conn ->
    let conn = Result.ok_exn conn in
    Deferred.upon
      (Rpc.Connection.close_reason conn)
      (fun reason ->
         debug "Connection in %s{.in/.out} is closed : %s"
           named_pipe
           (Info.to_string_hum reason)
      );
    return conn
  in
  let error = exec_in_namespace (parameters:Oci_Wrapper_Api.parameters) in
  return (error,conn)

type set_cpuset = {
  cgroup: string;
  cpuset: Int.t list;
} [@@deriving bin_io]

let set_cpuset = Rpc.Rpc.create
    ~name:"Oci_Artefact.set_cpuset"
    ~version:1
    ~bin_query:bin_set_cpuset
    ~bin_response:Unit.bin_t

(** runner to master *)

type rpc_create_query = {
  src: Oci_Filename.t;
  prune: Oci_Filename.t list;
  rooted_at: Oci_Filename.t;
  only_new: bool;
} [@@deriving sexp, bin_io]

let rpc_create =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.create"
    ~version:1
    ~bin_query:bin_rpc_create_query
    ~bin_response:Artefact.bin_t

type rpc_link_to_query =
  Oci_Common.user_kind * Artefact.t * Oci_Filename.t [@@deriving bin_io]

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

let rpc_get_internet =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.get_internet"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:Unit.bin_t

type rpc_git_clone_query = {
  url : String.t;
  dst: Oci_Filename.t;
  user: Oci_Common.user_kind;
  commit: Oci_Common.Commit.t;
} [@@deriving bin_io]

let rpc_git_clone =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.git_clone"
    ~version:1
    ~bin_query:bin_rpc_git_clone_query
    ~bin_response:Unit.bin_t

type rpc_git_copy_file_query = {
  url : String.t;
  src: Oci_Filename.t;
  dst: Oci_Filename.t;
  user: Oci_Common.user_kind;
  commit: Oci_Common.Commit.t;
} [@@deriving bin_io]

let rpc_git_copy_file =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.git_copy_file"
    ~version:1
    ~bin_query:bin_rpc_git_copy_file_query
    ~bin_response:Unit.bin_t

type rpc_get_file = {
  kind : [`MD5];
  checksum: String.t;
  dst : Oci_Filename.t;
} [@@deriving bin_io]

let rpc_get_file =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.get_file"
    ~version:1
    ~bin_query:bin_rpc_get_file
    ~bin_response:Unit.bin_t

(** ask a runner to stop nicely *)
let rpc_stop_runner =
  Rpc.Rpc.create
    ~name:"Oci_Runner.stop_runner"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:Unit.bin_t

let rpc_get_or_release_proc =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.get_or_release_proc"
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_response:Int.bin_t

let rpc_give_external_access =
  Rpc.Rpc.create
    ~name:"Oci_Artefact.give_external_access"
    ~version:1
    ~bin_query:Oci_Filename.bin_t
    ~bin_response:Oci_Filename.bin_t


let oci_at_shutdown,oci_shutdown,oci_shutting_down =
  let s = Stack.create () in
  let r = ref false in
  (fun d -> Stack.push s d),
  (fun status ->
     if status = 0
     then debug "Shutting down"
     else error "Shutting down unexpectedly";
     if !r then begin
       debug "Already shutting down";
       never ()
     end
     else begin
         r := true;
         s
         |> Stack.fold ~init:[] ~f:(fun acc d -> d ()::acc)
         |> Deferred.all_unit
         >>= fun () ->
         debug "Shutting down tasks finished";
         Log.Global.flushed ()
         >>= fun () ->
         Shutdown.exit status
     end
  ),
  (fun () -> !r)
