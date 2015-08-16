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

let (>>!) or_error_deferred f =
  or_error_deferred >>= fun r -> f (ok_exn r)

exception RunnerFailed of Unix.Exit_or_signal.error with sexp

let get_etc_sub_config ~user ~file =
  file
  |> Reader.open_file
  >>= fun reader ->
  reader
  |> Reader.lines
  |> Pipe.filter_map
    ~f:(fun s ->
        match String.split s ~on:':' with
        | [u;start;len] when String.equal u user ->
          Some (Int.of_string start,Int.of_string len)
        | _ -> None
      )
  |> Pipe.read
  >>= function
  | `Eof ->
    Printf.eprintf
      "This user doesn't have subuid or subgid configured (cf %s) \n%!"
      file;
    Shutdown.exit 1
  | `Ok start_len -> return start_len

let stderr_pipe = Writer.pipe (Lazy.force Writer.stderr)

let send_to_stderr reader =
  let reader = Reader.pipe reader in
  Pipe.transfer_id reader stderr_pipe

let send_process_to_stderr process =
  don't_wait_for (send_to_stderr (Process.stdout process));
  don't_wait_for (send_to_stderr (Process.stderr process))

let oci_wrapper =
  Oci_Filename.concat
    (Oci_Filename.dirname Sys.executable_name)
    "Oci_Wrapper.native"

let oci_simple_exec =
  Oci_Filename.concat
    (Oci_Filename.dirname Sys.executable_name)
    "Oci_Simple_Exec.native"


let exec_in_namespace parameters =
  let open Oci_Wrapper_Api in
  Process.create ~prog:oci_wrapper ~args:[] ()
  >>! fun process ->
  debug "Oci_Wrapper started";
  send_process_to_stderr process;
  Writer.write_bin_prot (Process.stdin process)
    bin_writer_parameters
    parameters;
  debug "Oci_Wrapper configuration sent";
  Process.wait process
  >>= function
  | Ok () -> return Oci_Artefact_Api.Exec_Ok
  | Error _ as s ->
    error "The following program stopped with this error %s:\n%s\n%!"
      (Unix.Exit_or_signal.to_string_hum s)
      (Sexp.to_string_hum (Oci_Wrapper_Api.sexp_of_parameters parameters));
    return (Oci_Artefact_Api.Exec_Error (Unix.Exit_or_signal.to_string_hum s))


type conf = {
  current_user: user;
  superroot: user;
  root: user;
  user: user;
  mutable conn_to_artefact: Rpc.Connection.t option;
}

(* let run_in_namespace ?(runas={uid=0;gid=0}) conf prog args
   : unit Deferred.t = *)
(*   Rpc.Rpc.dispatch_exn Oci_Simple_Exec_Api.run *)
(*     conf.conn_to_exec *)
(*     {Oci_Simple_Exec_Api.prog; args; runas} *)
(*   >>= fun r -> *)
(*   return (ok_exn r) *)

let compute_conf ~oci_data =
  User_and_group.for_this_process_exn ()
  >>= fun ug ->
  let current_user = {uid=Unix.getuid ();gid=Unix.getgid ()} in
  get_etc_sub_config ~user:(User_and_group.user ug) ~file:"/etc/subuid"
  >>= fun (ustart, ulen) ->
  get_etc_sub_config ~user:(User_and_group.user ug) ~file:"/etc/subgid"
  >>= fun (gstart, glen) ->
  if ulen < 1001 || glen < 1001 then begin
    Printf.eprintf
      "This user doesn't have enough subuid or \
       subgid configured (1001 needed)\n%!";
    Shutdown.exit 1
  end
  else
    let superroot = {uid=ustart;gid=gstart} in
    let root = {uid=ustart+1;gid=gstart+1} in
    let user = {uid=ustart+1001;gid=gstart+1001} in
    let conf = {current_user;superroot;root;user;conn_to_artefact=None} in
    Async_shell.run "mkdir" ["-p";"--";oci_data]
    >>= fun () ->
    Unix.chmod ~perm:0o777 oci_data
    >>= fun () ->
    return conf


let start_master ~conf ~master ~oci_data ~binaries =
  let open Oci_Wrapper_Api in
  let named_pipe = Oci_Filename.concat oci_data "oci_master" in
  let parameters = {
    rootfs = None;
    uidmap = [
      {extern_id=conf.superroot.uid; intern_id=0; length_id=1};
      {extern_id=conf.root.uid; intern_id=1; length_id=1};
      {extern_id=conf.user.uid; intern_id=2; length_id=1};
             ];
    gidmap = [
      {extern_id=conf.superroot.gid; intern_id=0; length_id=1};
      {extern_id=conf.root.gid; intern_id=1; length_id=1};
      {extern_id=conf.user.gid; intern_id=2; length_id=1};
             ];
    command = master;
    argv = [named_pipe];
    env = ["PATH","/usr/local/bin:/usr/bin:/bin"];
    runuid = 0;
    rungid = 0;
    bind_system_mount = false;
    prepare_network = false;
    workdir = None;
  } in
  let implementations = Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Raise
      ~implementations:[
        Rpc.Rpc.implement Oci_Artefact_Api.get_configuration
          (fun () () -> return {Oci_Artefact_Api.binaries;
                                oci_data;
                                oci_simple_exec;
                                superroot = conf.superroot;
                                root = conf.root;
                                user = conf.user;
                               });
        Rpc.Rpc.implement Oci_Artefact_Api.exec_in_namespace
          (fun () -> exec_in_namespace)
      ]
  in
  info "Start master";
  Oci_Artefact_Api.start_in_namespace ~exec_in_namespace ~parameters
    ~named_pipe
    ~implementations
    ()
  >>= fun (error,conn) ->
  choose [
    choice error (function
        | Exec_Ok ->
          info "master stopped unexpectedly but normally";
          Shutdown.shutdown 1
        | Exec_Error s ->
          info "master stopped unexpectedly with error:%s" s;
          Shutdown.shutdown 1
      );
    choice begin
      conn >>= fun conn ->
      Shutdown.at_shutdown (fun () -> Rpc.Connection.close conn);
      never ()
    end (fun _ -> ());
  ]



let run master binaries oci_data verbosity () =
  Log.Global.set_level verbosity;
  compute_conf ~oci_data
  >>= fun conf ->
  start_master ~conf ~binaries ~oci_data ~master

let () = Command.run begin
    let current_work_dir = Caml.Sys.getcwd () in
    let map_to_absolute =
      Command.Spec.map_flag ~f:begin
        fun f ->
          if Oci_Filename.is_relative f
          then Oci_Filename.make_absolute current_work_dir f
          else f
      end
    in
    Command.async_basic
      ~summary:"Start OCI continous integration framework"
      Command.Spec.(
        empty +>
        flag "--master" (map_to_absolute (required file))
          ~doc:" Specify the master to use" +>
        flag "--binaries" (map_to_absolute (required file))
          ~doc:" Specify where the runners are" +>
        flag "--oci-data" (map_to_absolute (required file))
          ~doc:" Specify where the OCI should store its files" +>
        flag "--verbosity" (map_flag ~f:Log.Level.of_string
                              (optional_with_default "Info" string))
          ~doc:" Specify the verbosity level (Debug,Error,Info)"
      )
      run
  end


(*

let id_runner = ref (-1)


    (* let open Oci_Wrapper_Api in *)
    (* return (conf, { *)
    (*     rootfs = None; (\** dumb *\) *)
    (*     command = ""; (\** dumb *\) *)
    (*     uidmap = [{extern_id=current_uid; intern_id=0; length_id=1}]; *)
    (*     gidmap = [{extern_id=current_gid; intern_id=0; length_id=1}]; *)
    (*     argv = []; *)
    (*     env = *)
    (*       ["PATH", *)
    (*        "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"; *)
    (*        "HOME","/root"]; *)
    (*     runuid = 0; *)
    (*     rungid = 0; *)
    (*     bind_system_mount = false; *)
    (*     prepare_network = true; *)
    (*   }) *)

let start_runner (data: ('q,'r) Oci_Data.t) (query: 'q) : 'r Deferred.t =
  let id = incr id_runner; !id_runner in
  info "Start runner %i for %s" id (Oci_Data.name data);
  conf
  >>= fun (conf,runner_namespace_parameters) ->
  let name = Oci_Data.name data in
  let rootfs = Printf.sprintf "oci_data/runner_%i/" id in
  Oci_Artefact.run_in_namespace conf
    "mkdir" ["-m";"777";"-p";"--";Oci_Filename.concat rootfs "oci"]
  >>= fun () ->
  let socket = Oci_Filename.concat rootfs "oci/oci.socket" in
  let result = Ivar.create () in
  Tcp.Server.create (Tcp.on_file socket)
    (fun _ reader writer ->
      Rpc.Connection.create
        ~connection_state:(fun _ -> ())
        ~implementations:!masters
        reader
        writer
      >>= fun conn ->
      Rpc.Rpc.dispatch_exn (Oci_Data.rpc data) (Result.ok_exn conn) query
      >>= fun r ->
      Ivar.fill result r;
      never ()
    )
  >>= fun server ->
  Unix.chmod socket ~perm:0o777
  >>= fun () ->
  Process.create ~prog:(get_binary "Oci_Wrapper") ~args:[] ()
  >>! fun process ->
  send_process_to_stderr process;
  let open Oci_Wrapper_Api in
  Writer.write_bin_prot (Process.stdin process)
    bin_writer_parameters
    {runner_namespace_parameters
     with rootfs = Some rootfs;
          command = get_binary name};
  Process.wait process
  >>= fun r ->
  Tcp.Server.close server
  >>= fun () ->
  Oci_Artefact.remove_dir conf rootfs;
  >>= fun () ->
  info "Stop runner for %s" (Oci_Data.name data);
  match r with
  | Ok () -> Ivar.read result
  | Error r -> raise (RunnerFailed r)


  (* >>= fun () -> *)
  (* Process.create ~prog:oci_wrapper ~args:[] () *)
  (* >>! fun process -> *)
  (* send_process_to_stderr process; *)
  (* don't_wait_for begin *)
  (*   Process.wait process *)
  (*   >>= function *)
  (*   | Ok () -> Tcp.Server.close server *)
  (*   | Error s -> *)
  (*     error "This program stopped with a failure:%s\n%!" *)
  (*       (Sexp.to_string_hum
           (Oci_Wrapper_Api.sexp_of_parameters parameters)); *)
  (*     Ivar.fill wait_error s; *)
  (*     Tcp.Server.close server *)
  (* end; *)
  (* let open Oci_Wrapper_Api in *)
  (* Writer.write_bin_prot (Process.stdin process) *)
  (*   bin_writer_parameters *)
  (*   parameters; *)
*)
