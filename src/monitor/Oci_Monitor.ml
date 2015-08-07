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

let binary_dir = ref Oci_Filename.current_dir
let current_uid = Unix.getuid ()
let current_gid = Unix.getgid ()

let get_binary name =
  Oci_Filename.concat !binary_dir (Oci_Filename.add_extension name "native")

let masters =
  ref (Rpc.Implementations.create_exn
         ~implementations:[] ~on_unknown_rpc:`Continue)

let register_master data f =
  masters := Rpc.Implementations.add_exn !masters
      (Rpc.Rpc.implement (Oci_Data.rpc data)
         (fun () q ->
           info "Test %s" (Oci_Data.name data);
           f q))

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

let start_simple_exec ~superroot ~root ~user =
  let socket = "oci_simple_exec_socket" in
  let conn = Ivar.create () in
  Tcp.Server.create (Tcp.on_file socket)
    (fun _ reader writer ->
      Rpc.Connection.create
        ~connection_state:(fun _ -> ())
        reader
        writer
      >>= fun c ->
      Ivar.fill conn (Result.ok_exn c);
      never ()
    )
  >>= fun server ->
  Shutdown.at_shutdown (fun () -> Tcp.Server.close server);
  Unix.chmod socket ~perm:0o777
  >>= fun () ->
  Process.create ~prog:(get_binary "Oci_Wrapper") ~args:[] ()
  >>! fun process ->
  send_process_to_stderr process;
  don't_wait_for begin
    Process.wait process
    >>= function
    | Ok () -> return ()
    | Error _ ->
      Printf.eprintf "Error: Oci_Simple_Exec stopped with a failure\n%!";
      Shutdown.exit 1
  end;
  let open Oci_Wrapper_Api in
  let parameters = {
    rootfs = None;
    uidmap = [{extern_id=superroot.uid; intern_id=0; length_id=1};
              {extern_id=root.uid; intern_id=1; length_id=1};
              {extern_id=user.uid; intern_id=1000; length_id=1};
             ];
    gidmap = [{extern_id=superroot.gid; intern_id=0; length_id=1};
              {extern_id=root.gid; intern_id=1; length_id=1};
              {extern_id=user.gid; intern_id=1000; length_id=1};
             ];
    command = get_binary "Oci_Simple_Exec";
    argv = [];
    env = ["PATH","/usr/local/bin:/usr/bin:/bin"];
    runuid = 0;
    rungid = 0;
    bind_system_mount = false;
    prepare_network = false;
  } in
  Writer.write_bin_prot (Process.stdin process)
    bin_writer_parameters
    parameters;
  Ivar.read conn

let conf =
  User_and_group.for_this_process_exn ()
  >>= fun ug ->
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
    start_simple_exec ~superroot ~root ~user
    >>= fun simple_exec_conn ->
    info "Simple_exec started";
    let conf = Oci_Artefact.create_conf
      ~storage:"oci_data/storage"
      ~superroot
      ~root
      ~user
      ~simple_exec_conn in
    Oci_Artefact.run_in_namespace conf "rm" ["-rf";"oci_data"]
    >>= fun () ->
    Oci_Artefact.run_in_namespace conf "mkdir" ["oci_data"]
    >>= fun () ->
    let open Oci_Wrapper_Api in
    return (conf, {
        rootfs = None; (** dumb *)
        command = ""; (** dumb *)
        uidmap = [{extern_id=current_uid; intern_id=0; length_id=1}];
        gidmap = [{extern_id=current_gid; intern_id=0; length_id=1}];
        argv = [];
        env =
          ["PATH",
           "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
           "HOME","/root"];
        runuid = 0;
        rungid = 0;
        bind_system_mount = false;
        prepare_network = true;
      })

let id_runner = ref (-1)

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


let run () =
  begin
    Rpc.Connection.serve
      ~where_to_listen:(Tcp.on_file "oci.socket")
      ~initial_connection_state:(fun _ _ -> ())
      ~implementations:!masters
      ()
    >>> fun server ->
    Shutdown.at_shutdown (fun () -> Tcp.Server.close server)
  end;
  Scheduler.go ()
