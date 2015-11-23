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

let create_dir_and_run
    t (q:Oci_Generic_Masters_Api.CompileGitRepoRunner.Query.t) =
  let working_dir = "/checkout" in
  Oci_Runner.cha_log t "Link Rootfs";
  Oci_Runner.link_artefact t q.rootfs.rootfs ~dir:"/"
  >>= fun () ->
  Oci_Runner.cha_log t "Link Artefacts";
  Deferred.List.iter
    ~f:(fun artefact ->
        Oci_Runner.link_artefact t artefact ~dir:"/"
      ) q.artefacts
  >>= fun () ->
  Oci_Runner.cha_log t "Clone repository at %s"
    (Oci_Common.Commit.to_string q.commit);
  Oci_Runner.git_clone t
    ~user:Root
    ~url:q.url
    ~dst:working_dir
    ~commit:q.commit
  >>= fun () ->
  Oci_Runner.cha_log t "Compile and install";
  let failures = Queue.create () in
  Deferred.List.iter
    ~f:(fun cmd ->
        Oci_Runner.get_release_proc t cmd.proc_requested
          (fun got ->
             let args =
               List.map cmd.args
                 ~f:(function
                     | `S s -> s
                     | `Proc -> string_of_int got)
             in
             Oci_Runner.run t ~working_dir ~prog:cmd.cmd ~args
               ~env:(cmd.env :> Async.Std.Process.env) ()
             >>= fun r ->
             match cmd.kind, r with
             | _, Core_kernel.Std.Result.Ok () -> return ()
             | `Required, Core_kernel.Std.Result.Error _ ->
               raise Oci_Runner.CommandFailed
             | `Test, _ ->
               Queue.enqueue failures (Oci_Runner.print_cmd cmd.cmd args);
               return ()
          )
      ) q.cmds
  >>= fun () ->
  return (working_dir,Queue.to_list failures)


let compile_git_repo_runner t q =
  create_dir_and_run t q
  >>= fun (working_dir,failures) ->
  Oci_Runner.create_artefact t
    ~dir:"/"
    ~prune:[working_dir]
  >>= fun artefact -> return {
    Oci_Generic_Masters_Api.CompileGitRepoRunner.Result.artefact;
    failures;
  }

(*
let tmux_runner t q =
  create_dir_and_run t q
  >>= fun (working_dir,_) ->
  let tmux_socket = Oci_Filename.make_absolute working_dir "tmux_socket" in
  Oci_Runner.cha_log t "Run TMux";
  Oci_Runner.run_exn t ~working_dir ~prog:"ls"
    ~args:["-l";"/"] ()
  >>= fun () ->
  (* Oci_Runner.run_exn t ~working_dir ~prog:"strace" *)
  (*   ~args:["-f";"-v";"tmux";"-vvv"; *)
  (*          "-S";tmux_socket;"start-server";";";"new-session";"-d"; *)
  (*          ";";"wait-for";"test"] () *)
  (* >>= fun () -> *)
  (* Oci_Runner.run_exn t ~working_dir ~prog:"tmux" *)
  (*   ~args:["-vvv"; *)
  (*          "-S";tmux_socket;"start-server";";";"new-session";"-d"; *)
  (*          ";";"wait-for";"test"] () *)
  Unix.mkdir ~p:() ~perm:0o777 (Oci_Filename.concat working_dir ".xpra")
  >>= fun () ->
  Oci_Runner.run_exn t ~working_dir ~prog:"xpra"
    ~args:["start";":7";"--no-daemon";"--socket-dir";working_dir;"--no-mmap";
           "--start-child=xterm";"--exit-with-children";"--no-mdns";
           "--no-notifications";"--no-speaker";"--no-microphone"] ()
  >>= fun () ->
  (* Unix.chmod tmux_socket ~perm:0o666 *)
  (* >>= fun () -> *)
  (* Oci_Runner.run_exn t ~working_dir ~prog:"tmux" *)
  (*   ~args:["-S";tmux_socket;"has";";";"wait-for";"test"] () *)
  (* >>= fun () -> *)
  Oci_Runner.give_external_access t tmux_socket
  *)

let xpra_runner t q =
  create_dir_and_run t q
  >>= fun (working_dir,_) ->
  let xpra_dir = Oci_Filename.make_absolute working_dir "xpra_socket" in
  Unix.mkdir ~p:() ~perm:0o777 xpra_dir
  >>= fun () ->
  Unix.mkdir ~p:() ~perm:0o777
    (Oci_Filename.make_absolute working_dir ".xpra")
  >>= fun () ->
  let xpra =
    Oci_Runner.run_exn t
      ~working_dir
      ~prog:"xpra"
      ~args:["start";":100";"--no-daemon";"--socket-dir";xpra_dir;
             "--start-child=xterm";"--exit-with-children";
             "--no-mmap";"--no-mdns";
             "--no-notifications";
             "--no-speaker";"--no-microphone"]
      ~env:(`Extend ["XPRA_SOCKET_HOSTNAME","oci"])
      ()
  in
  let xpra_socket = Oci_Filename.make_absolute xpra_dir "oci-100" in
  Sys.when_file_exists xpra_socket
  >>= fun () ->
  Unix.chmod ~perm:0o666 xpra_socket
  >>= fun () ->
  Oci_Runner.give_external_access t xpra_dir
  >>= fun external_socket ->
  Oci_Runner.cha_log t
    "Run: XPRA_SOCKET_HOSTNAME=oci xpra attach :100 --socket-dir %s"
    external_socket;
  xpra
  >>= fun () ->
  return external_socket


let () =
  never_returns begin
    Oci_Runner.start
      ~implementations:[
        Oci_Runner.implement
          Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc
          compile_git_repo_runner;
        Oci_Runner.implement
          Oci_Generic_Masters_Api.XpraRunner.rpc
          xpra_runner;
      ]
  end
