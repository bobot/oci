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

type conf = {
  mutable next_artefact_id: Int.t;
  mutable next_runner_id: Int.t;
  conn_monitor : Rpc.Connection.t;
  binaries: Oci_Filename.t;
  storage: Oci_Filename.t;
  runners: Oci_Filename.t;
  permanent: Oci_Filename.t;
  (** permanent storage for masters *)
  conf_monitor: Oci_Artefact_Api.artefact_api;
  api_for_runner: Oci_Filename.t Rpc.Implementations.t;
}

let gconf = ref None

type t = Oci_Common.Artefact.t

exception Directory_should_not_exists of Oci_Filename.t

let get_conf () =
  Option.value_exn
    ~message:"Configuration can't be used before starting the `run` function"
    !gconf

let dir_of_artefact id =
  let dir = Oci_Filename.mk (Artefact.to_string id) in
  Oci_Filename.make_absolute (get_conf ()).storage dir

let create src =
  let conf = get_conf () in
  conf.next_artefact_id <- conf.next_artefact_id + 1;
  let id = Artefact.of_int conf.next_artefact_id in
  let dst = dir_of_artefact id in
  Sys.file_exists_exn (Oci_Filename.get dst)
  >>= fun b ->
  if b then raise (Directory_should_not_exists dst);
  Async_shell.run "cp" ["-a";"--";
                              Oci_Filename.get src;
                              Oci_Filename.get dst]
  >>= fun () ->
  Async_shell.run "chown" ["-R";
                           pp_chown (master_user Superroot);
                           Oci_Filename.get dst]
  >>=
  fun () -> return id

let link_to id dst =
  let src = dir_of_artefact id in
  Async_shell.run "rm" ["-rf";"--";Oci_Filename.get dst]
  >>= fun () ->
  Async_shell.run "cp" ["-rla";"--";
                              Oci_Filename.get src;
                              Oci_Filename.get dst]

let copy_to id dst =
  let src = dir_of_artefact id in
  Async_shell.run "rm" ["-rf";"--";Oci_Filename.get dst]
  >>= fun () ->
  Async_shell.run "cp" ["-a";"--";
                              Oci_Filename.get src;
                              Oci_Filename.get dst]

let is_available id =
  let src = dir_of_artefact id in
  Sys.file_exists_exn (Oci_Filename.get src)

let remove_dir dir =
  Async_shell.run "rm" ["-rf";"--"; Oci_Filename.get dir]

let permanent_directory data =
  let conf = get_conf () in
  let dir =
    Oci_Filename.make_absolute conf.permanent
      (Oci_Filename.concat (Oci_Data.name data)
         (Int.to_string (Oci_Data.version data))) in
  Async_shell.run "mkdir" ["-p";"--";dir]
  >>= fun () ->
  return dir

let artifact_data_permanent_file conf =
  let dir = Oci_Filename.make_absolute conf.permanent "Oci_Artefact_Api" in
  Async_shell.run "mkdir" ["-p";"--";dir]
  >>= fun () ->
  return (Oci_Filename.concat dir "data")

let loader_artifact_data () =
  let conf = get_conf () in
  artifact_data_permanent_file conf
  >>= fun file ->
  Oci_Std.read_if_exists file Int.bin_reader_t
    (fun r -> conf.next_artefact_id <- r; return ())

let saver_artifact_data () =
  let conf = get_conf () in
  artifact_data_permanent_file conf
  >>= fun file ->
  Oci_Std.backup_and_open_file file
  >>= fun writer ->
  Writer.write_bin_prot writer Int.bin_writer_t conf.next_artefact_id;
  Writer.close writer

(* let create_conf ~storage ~superroot ~root ~user ~simple_exec_conn = *)
(*   {storage; superroot; root; user; conn = simple_exec_conn} *)

(** {2 Management} *)
let masters =
  ref (Rpc.Implementations.create_exn
         ~implementations:[] ~on_unknown_rpc:`Close_connection)

let register_master data f =
  let name = (Printf.sprintf "Master %s" (Oci_Data.name data)) in
  masters := Rpc.Implementations.add_exn !masters
      (Rpc.Rpc.implement (Oci_Data.rpc data)
         (fun rootfs q ->
            debug "%s called from %s" name rootfs;
            Monitor.protect ~name
              ~finally:(fun () -> return ())
              (fun () -> f q)))

let savers = Stack.create ()

let register_saver ~loader ~saver =
  Stack.push savers (loader,saver)

let save =
  let sequence = Sequencer.create ~continue_on_error:true () in
  let f () =
    info "Save masters data";
    savers
    |> Stack.fold ~f:(fun acc (_,f) -> f ()::acc) ~init:[]
    |> Deferred.all_unit
  in
  fun () -> Throttle.enqueue sequence f

let load () =
  info "Load masters data";
  savers
  |> Stack.fold ~f:(fun acc (f,_) -> f ()::acc) ~init:[]
  |> Deferred.all_unit

let exec_in_namespace parameters =
  Rpc.Rpc.dispatch_exn
    Oci_Artefact_Api.exec_in_namespace
    (get_conf ()).conn_monitor
    parameters

let add_artefact_api init =
  List.fold_left ~f:Rpc.Implementations.add_exn ~init [
    (** create *)
    Rpc.Rpc.implement
      Oci_Artefact_Api.rpc_create
      (fun rootfs src ->
         assert (not (Oci_Filename.is_relative src));
         let src = Oci_Filename.make_relative "/" src in
         let src = Oci_Filename.make_absolute rootfs src in
         create src
      );
    (** link_to *)
    Rpc.Rpc.implement
      Oci_Artefact_Api.rpc_link_to
      (fun rootfs (artefact,dst) ->
         assert (not (Oci_Filename.is_relative dst));
         let dst = Oci_Filename.make_relative "/" dst in
         let dst = Oci_Filename.make_absolute rootfs dst in
         link_to artefact dst
      );
    (** copy_to *)
    Rpc.Rpc.implement
      Oci_Artefact_Api.rpc_copy_to
      (fun rootfs (artefact,dst) ->
         assert (not (Oci_Filename.is_relative dst));
         let dst = Oci_Filename.make_relative "/" dst in
         let dst = Oci_Filename.make_absolute rootfs dst in
         copy_to artefact dst
      )
  ]

let start_runner ~binary_name =
  let conf = get_conf () in
  conf.next_runner_id <- conf.next_runner_id + 1;
  let runner_id = conf.next_runner_id in
  let rootfs = Oci_Filename.concat (get_conf ()).runners
      (Oci_Filename.mk (string_of_int runner_id)) in
  Unix.mkdir ~p:() (Oci_Filename.concat rootfs "oci")
  >>= fun () ->
  let etc = (Oci_Filename.concat rootfs "etc") in
  Unix.mkdir ~p:() etc
  >>= fun () ->
  Async_shell.run "cp" ["/etc/resolv.conf";"-t";etc]
  >>= fun () ->
  Async_shell.run "chown" [pp_chown (master_user Root);"-R";"--";rootfs]
  >>= fun () ->
  let binary =
    Oci_Filename.concat (get_conf ()).binaries
      (Oci_Filename.add_extension binary_name "native") in
  let named_pipe = Oci_Filename.concat "oci" "oci_runner" in
  let parameters : Oci_Wrapper_Api.parameters = {
    rootfs = Some rootfs;
    idmaps =
      Oci_Wrapper_Api.idmaps
        ~first_user_mapped:conf.conf_monitor.first_user_mapped
        ~in_user:runner_user
        [Root,1000;User,1];
    command = binary;
    argv = [named_pipe];
    env = ["PATH","/usr/local/bin:/usr/bin:/bin"];
    runuid = 0;
    rungid = 0;
    bind_system_mount = false;
    prepare_network = false;
    workdir = None;
  } in
  info "Start runner %s" binary_name;
  let r =
    Oci_Artefact_Api.start_in_namespace
      ~exec_in_namespace ~parameters
      ~implementations:conf.api_for_runner
      ~initial_state:rootfs
      ~named_pipe:(Oci_Filename.concat rootfs named_pipe) () in
  begin
    r
    >>> fun (result,_) ->
    result
    >>> fun _ ->
    Async_shell.run "rm" ["-rf";"--";rootfs]
    >>> fun () ->
    ()
  end;
  r

let conn_monitor () =
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:[
        Rpc.Rpc.implement Oci_Artefact_Api.rpc_stop_runner
          (fun () () -> Oci_Artefact_Api.oci_shutdown ())
      ]
      ~on_unknown_rpc:`Raise in
  let named_pipe = Sys.argv.(1) in
  Reader.open_file (named_pipe^".in")
  >>= fun reader ->
  Writer.open_file (named_pipe^".out")
  >>= fun writer ->
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

let run () =
  info "Run Artefact";
  begin
    conn_monitor ()
    >>> fun conn_monitor ->
    Rpc.Rpc.dispatch_exn Oci_Artefact_Api.get_configuration conn_monitor ()
    >>> fun conf_monitor ->
    let conf = {
      next_artefact_id = -1;
      next_runner_id = -1;
      conn_monitor;
      runners = Oci_Filename.concat conf_monitor.oci_data "runners";
      binaries = Oci_Filename.concat conf_monitor.oci_data "binaries";
      storage = Oci_Filename.concat conf_monitor.oci_data "storage";
      permanent = Oci_Filename.concat conf_monitor.oci_data "permanent";
      conf_monitor;
      api_for_runner = add_artefact_api !masters;
    } in
    gconf := Some conf;
    if conf_monitor.debug_level then Log.Global.set_level `Debug;
    Async_shell.run "rm" ["-rf";"--";conf.runners;conf.binaries]
    >>> fun () ->
    Deferred.all_unit (List.map ~f:(Unix.mkdir ~p:() ?perm:None)
                         [conf.runners;conf.binaries;
                          conf.storage;conf.permanent])
    >>> fun () ->
    (** Copy binaries *)
    Sys.ls_dir conf_monitor.binaries
    >>> fun files ->
    let files = List.filter_map
        ~f:(fun f -> if String.is_suffix f ~suffix:"native"
             then Some f else None)
        files in
    begin if files = [] then return ()
    else
      Async_shell.run "cp" (["-t";conf.binaries;"--"]@
                            List.map
                              ~f:(Oci_Filename.concat conf_monitor.binaries)
                              files)
    end
    >>> fun () ->
    Deferred.all_unit
      (List.map
         ~f:(fun x -> Unix.chmod ~perm:0o555
                (Oci_Filename.concat conf.binaries x))
         files)
    >>> fun () ->
    register_saver ~loader:loader_artifact_data ~saver:saver_artifact_data;
    load ()
    >>> fun () ->
    let save_at = Time.Span.create ~min:10 () in
    Clock.every' ~start:(after save_at) save_at save;
    Oci_Artefact_Api.oci_at_shutdown save;
    let socket = Oci_Filename.concat conf_monitor.oci_data "oci.socket" in
    Async_shell.run "rm" ["-f";"--";socket]
    >>> fun () ->
    Rpc.Connection.serve
      ~where_to_listen:(Tcp.on_file socket)
      ~initial_connection_state:(fun _ _ -> "external socket")
      ~implementations:!masters
      ()
    >>> fun server ->
    Shutdown.at_shutdown (fun () -> Tcp.Server.close server);
    Unix.chmod socket ~perm:0o777
    >>> fun () ->
    ()
  end;
  Scheduler.go ()
