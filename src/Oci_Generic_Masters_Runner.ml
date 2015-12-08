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

open Oci_Generic_Masters_Api.CompileGitRepoRunner

let create_dir t (q:Query.t) =
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
  Unix.mkdir ~p:() working_dir
  >>= fun () ->
  return working_dir

let run_cmds t kind working_dir = function
    | GitClone clone ->
      Oci_Runner.cha_log t "Clone repository at %s"
        (Oci_Common.Commit.to_string clone.commit);
      Oci_Runner.git_clone t
        ~user:Root
        ~url:clone.url
        ~dst:(Oci_Filename.make_absolute working_dir clone.directory)
        ~commit:clone.commit
    | Exec cmd ->
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
           match kind, r with
           | `Required, Core_kernel.Std.Result.Ok () -> return ()
           | `Test, Core_kernel.Std.Result.Ok () ->
             Oci_Runner.data_log t
                  (`Test (`Ok,(Oci_Runner.print_cmd cmd.cmd args)));
             return ()
           | `Required, Core_kernel.Std.Result.Error _ ->
             Oci_Runner.data_log t
                  (`Compilation (`Failed (Oci_Runner.print_cmd cmd.cmd args)));
             raise Oci_Runner.StopQuery
           | `Test, _ ->
             Oci_Runner.data_log t
                  (`Test (`Failed,(Oci_Runner.print_cmd cmd.cmd args)));
             return ()
        )


let compile_git_repo_runner t q =
  create_dir t q
  >>= fun working_dir ->
  Deferred.List.iter ~f:(run_cmds t `Required working_dir) q.cmds
  >>= fun () ->
  Oci_Runner.create_artefact t
    ~dir:"/"
    ~prune:[working_dir]
  >>= fun artefact ->
  Oci_Runner.data_log t (`Compilation (`Ok artefact));
  Deferred.List.iter ~f:(run_cmds t `Test working_dir) q.tests

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
  create_dir t q
  >>= fun working_dir ->
  Deferred.List.iter ~f:(run_cmds t `Required working_dir) q.cmds
  >>= fun () ->
  Deferred.List.iter ~f:(run_cmds t `Test working_dir) q.tests
  >>= fun () ->
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
  >>= fun external_dir ->
  let xpra_script = "remote-xpra.sh" in
  let remote_xpra = Oci_Filename.make_absolute xpra_dir xpra_script in
  Writer.open_file remote_xpra
  >>= fun writer ->
  Writer.writef writer "#!/bin/sh -ue\n\n";
  Writer.writef writer "export XPRA_SOCKET_HOSTNAME=oci\n";
  Writer.writef writer "export XPRA_SOCKET_DIR=%s\n\n" external_dir;
  Writer.writef writer "exec xpra  \"$@\"";
  Writer.close writer
  >>= fun () ->
  Unix.chmod ~perm:0o555 remote_xpra
  >>= fun () ->
  Oci_Runner.data_log t (`XpraDir external_dir);
  Oci_Runner.cha_log t
    "Run locally: XPRA_SOCKET_HOSTNAME=oci xpra attach :100 --socket-dir %S"
    external_dir;
  Oci_Runner.cha_log t
    "Run remotely: xpra attach --remote-xpra %S ssh:HOST:100"
    (Oci_Filename.make_absolute external_dir xpra_script);
  xpra


let () =
  never_returns begin
    Oci_Runner.start
      ~implementations:[
        Oci_Runner.implement_unit
          Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc
          compile_git_repo_runner;
        Oci_Runner.implement_unit
          Oci_Generic_Masters_Api.XpraRunner.rpc
          xpra_runner;
      ]
  end
