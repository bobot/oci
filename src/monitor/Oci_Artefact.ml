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

open Log.Global

let superroot = {uid=0;gid=0}
(** A user outside the usernamespace of the runners stronger than the
      root, In Artifact run with superroot as root *)

let root = {uid=1;gid=1}
(** root in the usernamespace of the runners *)

let user = {uid=1001;gid=1001}
(** A simple user in the usernamespace of the runners *)

let next_id = ref (-1)
let conn : Rpc.Connection.t option = None
(** connection to the monitor *)
let storage = ref "dumb"
let temporary = ref "dumb"

type t = Oci_Common.artefact with sexp
let bin_t = Oci_Common.bin_artefact

exception Directory_should_not_exists of Oci_Filename.t

let dir_of_id id =
  let dir = Oci_Filename.mk (string_of_int id) in
  Oci_Filename.make_absolute !storage dir


let create src =
  incr next_id;
  let id = !next_id in
  let dst = dir_of_id id in
  Sys.file_exists_exn (Oci_Filename.get dst)
  >>= fun b ->
  if not b then raise (Directory_should_not_exists dst);
  Unix.mkdir (Oci_Filename.get dst)
  >>= fun () ->
  Async_shell.run "cp" ["-a";"--";
                              Oci_Filename.get src;
                              Oci_Filename.get dst]
  >>= fun () ->
  Async_shell.run "chown" ["-R";
                                 Printf.sprintf "%i:%i"
                                   superroot.uid
                                   superroot.gid;
                                 Oci_Filename.get dst]
  >>=
  fun () -> return id

let link_to id dst =
  let src = dir_of_id id in
  Async_shell.run "rm" ["-rf";"--";Oci_Filename.get dst]
  >>= fun () ->
  Async_shell.run "cp" ["-rla";"--";
                              Oci_Filename.get src;
                              Oci_Filename.get dst]

let copy_to id dst =
  let src = dir_of_id id in
  Async_shell.run "rm" ["-rf";"--";Oci_Filename.get dst]
  >>= fun () ->
  Async_shell.run "cp" ["-a";"--";
                              Oci_Filename.get src;
                              Oci_Filename.get dst]

let is_available id =
  let src = dir_of_id id in
  Sys.file_exists_exn (Oci_Filename.get src)

let remove_dir dir =
  Async_shell.run "rm" ["-rf";"--"; Oci_Filename.get dir]

(* let create_conf ~storage ~superroot ~root ~user ~simple_exec_conn = *)
(*   {storage; superroot; root; user; conn = simple_exec_conn} *)

(** {2 Management} *)
  let binary_dir = ref Oci_Filename.current_dir
let current_uid = Unix.getuid ()
let current_gid = Unix.getgid ()

let masters =
  ref (Rpc.Implementations.create_exn
         ~implementations:[] ~on_unknown_rpc:`Continue)

let register_master data f =
  masters := Rpc.Implementations.add_exn !masters
      (Rpc.Rpc.implement (Oci_Data.rpc data)
         (fun () q ->
           info "Master %s" (Oci_Data.name data);
           f q))

let id_runner = ref (-1)

let conn_monitor =
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:[]
      ~on_unknown_rpc:`Raise in
  let named_pipe = Sys.argv.(1) in
  info "Before";
  Reader.open_file (named_pipe^".in")
  >>= fun reader ->
  info "Reader";
  Writer.open_file (named_pipe^".out")
  >>= fun writer ->
  info "Writer";
  Rpc.Connection.create
    ~implementations
    ~connection_state:(fun _ -> ())
    reader writer
  >>= fun conn ->
  let conn = Result.ok_exn conn in
  Shutdown.at_shutdown (fun () ->
      Rpc.Connection.close conn
      >>= fun () ->
      Reader.close reader;
      >>= fun () ->
      Writer.close writer
    );
  return conn

let conf =
  conn_monitor
  >>= fun conn_monitor ->
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.get_configuration conn_monitor ()

let binary_dir conf =
  Oci_Filename.concat conf.Oci_Artefact_Api.oci_data "binaries"

let get_binary conf name =
  Oci_Filename.concat
    (binary_dir conf)
    (Oci_Filename.add_extension name "native")

let exec_in_namespace parameters =
  conn_monitor
  >>= fun conn_monitor ->
  Rpc.Rpc.dispatch_exn
    Oci_Artefact_Api.exec_in_namespace conn_monitor
    parameters

let start_runner ~binary_name =
  conf >>= fun conf ->
  let open Oci_Wrapper_Api in
  let binary_in_namespace =
    Oci_Filename.concat "/oci" (Oci_Filename.add_extension binary_name "native")
  in
  let named_pipe =
    Oci_Filename.concat conf.Oci_Artefact_Api.oci_data "oci_master.socket"
  in
  let parameters = {
    rootfs = None;
    uidmap = [
      {extern_id=conf.Oci_Artefact_Api.root.uid; intern_id=0; length_id=1000};
      {extern_id=conf.Oci_Artefact_Api.user.uid; intern_id=1000; length_id=1};
             ];
    gidmap = [
      {extern_id=conf.Oci_Artefact_Api.root.gid; intern_id=0; length_id=1000};
      {extern_id=conf.Oci_Artefact_Api.user.gid; intern_id=1000; length_id=1};
             ];
    command = binary_in_namespace;
    argv = [named_pipe];
    env = ["PATH","/usr/local/bin:/usr/bin:/bin"];
    runuid = 0;
    rungid = 0;
    bind_system_mount = false;
    prepare_network = false;
    workdir = None;
  } in
  Oci_Artefact_Api.start_in_namespace
    ~exec_in_namespace ~parameters
    ~named_pipe ()

let run () =
  info "Run Artefact";
  begin
    conf
    >>> fun conf ->
    let socket = Oci_Filename.concat conf.oci_data "oci.socket" in
    Rpc.Connection.serve
      ~where_to_listen:(Tcp.on_file socket)
      ~initial_connection_state:(fun _ _ -> ())
      ~implementations:!masters
      ()
    >>> fun server ->
    Shutdown.at_shutdown (fun () -> Tcp.Server.close server);
    Unix.chmod socket ~perm:0o777
    >>> fun () ->
    ()
  end;
  Scheduler.go ()
