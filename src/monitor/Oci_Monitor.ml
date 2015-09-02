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

let () =
  Oci_Artefact_Api.oci_at_shutdown (fun () ->
      Deferred.ignore (Pipe.upstream_flushed stderr_pipe)
    )

let oci_wrapper =
  Oci_Filename.concat
    (Oci_Filename.dirname Sys.executable_name)
    "Oci_Wrapper.native"

let oci_simple_exec =
  Oci_Filename.concat
    (Oci_Filename.dirname Sys.executable_name)
    "Oci_Simple_Exec.native"

type process = {
  wrapper : Process.t;
  wrapped : Pid.t;
  (** We don't want to use [Process.wait wrapper] more than once,
      otherwise one receive an error not existing
  *)
  wrapper_wait : Unix.Exit_or_signal.t Deferred.t;
}


type conf = {
  current_user: user;
  first_user_mapped: user;
  mutable conn_to_artefact: Rpc.Connection.t option;
  mutable wait_to_artefact: unit Deferred.t option;
  wrappers_dir : Oci_Filename.t;
  running_processes: process Bag.t;
}


let cleanup_running_processes conf () =
  begin match conf.wait_to_artefact with
  | None ->
    info "No master to stop";
    return ()
  | Some wait ->
    let timeout = Time.Span.create ~sec:10 () in
    info "Master have %s to stop nicely" (Time.Span.to_string_hum timeout);
    Deferred.ignore
      (Clock.with_timeout timeout
         begin (match conf.conn_to_artefact with
             | None -> return ()
             | Some conn ->
               Rpc.Rpc.dispatch Oci_Artefact_Api.rpc_stop_runner conn ()
               >>= fun _ ->
               wait)
         end)
  end
  >>= fun () ->
  Bag.iter
    ~f:(fun process ->
        if not (Deferred.is_determined process.wrapper_wait) then begin
          debug "Send kill signal to %s" (Pid.to_string process.wrapped);
          Signal.send_i Signal.kill (`Pid process.wrapped)
        end)
    conf.running_processes;
  conf.running_processes
  |> Bag.fold
    ~f:(fun acc process ->
        (Deferred.ignore process.wrapper_wait)::acc)
    ~init:[]
  |> Deferred.all_unit

let exec_in_namespace =
  let wrapper_id = ref (-1) in
  fun conf parameters ->
    let open Oci_Wrapper_Api in
    incr wrapper_id;
    let named_pipe = Oci_Filename.make_absolute conf.wrappers_dir
        (Printf.sprintf "wrappers%i" !wrapper_id)  in
    let named_pipe_in = named_pipe^".in" in
    let named_pipe_out = named_pipe^".out" in
    Unix.mkfifo named_pipe_in
    >>= fun () ->
    Unix.mkfifo named_pipe_out
    >>= fun () ->
    Process.create ~prog:oci_wrapper ~args:[named_pipe] ()
    >>! fun wrapper ->
    debug "Oci_Wrapper started";
    (** possible race for the addition to running_process *)
    send_process_to_stderr wrapper;
    Writer.close (Process.stdin wrapper);
    >>= fun () ->
    Writer.open_file named_pipe_in
    >>= fun writer ->
    Unix.unlink named_pipe_in
    >>= fun () ->
    Writer.write_bin_prot writer
      bin_writer_parameters
      parameters;
    Writer.close writer
    >>= fun () ->
    debug "Oci_Wrapper configuration sent";
    Reader.open_file named_pipe_out
    >>= fun reader ->
    Unix.unlink named_pipe_out
    >>= fun () ->
    Reader.read_bin_prot reader Pid.bin_reader_t
    >>= function
    | `Eof ->
      Reader.close reader
      >>= fun () ->
      error "The pid of the wrapped program can't be read";
      return (Oci_Artefact_Api.Exec_Error "Wrapper error: can't get pid")
    | `Ok wrapped ->
      debug "Oci_Wrapper pid %s received" (Pid.to_string wrapped);
      let process_info = {
        wrapper; wrapped; wrapper_wait = Process.wait wrapper;
      } in
      let elt = Bag.add conf.running_processes process_info in
      Reader.close reader
      >>= fun () ->
      process_info.wrapper_wait
      >>= fun res ->
      debug "Wrapped program %s ended" (Pid.to_string wrapped);
      Bag.remove conf.running_processes elt;
      match res with
      | Ok () -> return Oci_Artefact_Api.Exec_Ok
      | Error _ as s ->
        error "The following program stopped with this error %s:\n%s\n"
          (Unix.Exit_or_signal.to_string_hum s)
          (Sexp.to_string_hum (Oci_Wrapper_Api.sexp_of_parameters parameters));
        return (Oci_Artefact_Api.Exec_Error
                  (Unix.Exit_or_signal.to_string_hum s))

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
    let first_user_mapped = {uid=ustart;gid=gstart} in
    let conf = {current_user;first_user_mapped;conn_to_artefact=None;
                wait_to_artefact=None;
                wrappers_dir =
                  Oci_Filename.make_absolute oci_data "wrappers";
                running_processes = Bag.create ()} in
    Async_shell.run "mkdir" ["-p";"--";oci_data]
    >>= fun () ->
    Unix.chmod ~perm:0o777 oci_data
    >>= fun () ->
    Async_shell.run "rm" ["-rf";"--";conf.wrappers_dir]
    >>= fun () ->
    Async_shell.run "mkdir" ["-p";"--";conf.wrappers_dir]
    >>= fun () ->
    return conf


let start_master ~conf ~master ~oci_data ~binaries ~verbosity ~cleanup_rootfs =
  let open Oci_Wrapper_Api in
  let named_pipe = Oci_Filename.concat oci_data "oci_master" in
  let parameters = {
    rootfs = None;
    idmaps =
      Oci_Wrapper_Api.idmaps
        ~first_user_mapped:conf.first_user_mapped
        ~in_user:master_user
        [Superroot,1;Root,1;User,1];
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
                                first_user_mapped = conf.first_user_mapped;
                                debug_level = verbosity = `Debug;
                                cleanup_rootfs;
                               });
        Rpc.Rpc.implement Oci_Artefact_Api.exec_in_namespace
          (fun () -> exec_in_namespace conf)
      ]
  in
  info "Start master";
  Oci_Artefact_Api.start_in_namespace
    ~exec_in_namespace:(exec_in_namespace conf) ~parameters
    ~named_pipe
    ~implementations
    ~initial_state:()
    ()
  >>= fun (error,conn) ->
  conf.wait_to_artefact <- Some (error >>= fun _ -> return ());
  choose [
    choice error (function
        | Exec_Ok when Oci_Artefact_Api.oci_shutting_down () ->
          info "master stopped for shutdown"
        | Exec_Ok ->
          info "master stopped unexpectedly but normally";
          Shutdown.shutdown 1
        | Exec_Error s ->
          info "master stopped unexpectedly with error:%s" s;
          Shutdown.shutdown 1
      );
    choice begin
      conn >>= fun conn -> conf.conn_to_artefact <- Some conn; never ()
    end (fun _ -> ());
  ]



let run master binaries oci_data verbosity cleanup_rootfs () =
  Log.Global.set_level verbosity;
  assert (not (Signal.is_managed_by_async Signal.term));
  (** Handle nicely terminating signals *)
  Signal.handle Signal.terminating ~f:(fun s ->
      if Oci_Artefact_Api.oci_shutting_down ()
      then
        info "Shutting down already started: monitor received %s"
          (Signal.to_string s)
      else begin
        info "Shutting down: monitor received %s" (Signal.to_string s);
        don't_wait_for (Oci_Artefact_Api.oci_shutdown ())
      end
    );
  compute_conf ~oci_data
  >>= fun conf ->
  Oci_Artefact_Api.oci_at_shutdown (cleanup_running_processes conf);
  start_master ~conf ~binaries ~oci_data ~master ~verbosity ~cleanup_rootfs

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
        flag "--verbosity" (optional_with_default `Info Log.Level.arg)
          ~doc:" Specify the verbosity level (Debug,Error,Info)" +>
        flag "--cleanup-rootfs" (optional_with_default true bool)
          ~doc:" For debug can be set to false for keeping rootfs after running"
      )
      run
  end
