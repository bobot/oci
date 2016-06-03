(**************************************************************************)
(*                                                                        *)
(*  This file is part of OCI.                                             *)
(*                                                                        *)
(*  Copyright (C) 2015-2016                                               *)
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
open Oci_Wrapper_Api
open ExtUnix.Specific
open Oci_Wrapper_Lib

let named_pipe_in = Sys.argv.(1)^".in"
let named_pipe_out = Sys.argv.(1)^".out"

let param : parameters =
  let cin = In_channel.create named_pipe_in in
  let param =
    Bin_prot.Utils.bin_read_stream
      ~read:(fun buf ~pos ~len ->
          Bigstring.really_input ~pos ~len cin buf)
      bin_reader_parameters
  in
  In_channel.close cin;
  param

let send_pid pid =
  let cout = Out_channel.create named_pipe_out in
  let buf = Bin_prot.Utils.bin_dump ~header:true Pid.bin_writer_t pid in
  Bigstring.really_output cout  buf;
  Out_channel.close cout

let () =
  if Unix.getuid () = 0 then begin
    Printf.eprintf "This program shouldn't be run as root!\n%!";
    exit 1
  end;
  Unix.handle_unix_error begin fun () ->
    (* remove the process from the group of the process monitor, and
    detach it from the controlling terminal. It allows to manage the
    shutdown nicely *)
    let _sessionid = Core.Std.Caml.Unix.setsid () in
    begin match param.cgroup with
    | None -> ()
    | Some cgroup ->
      let abs a s =
        let a = Oci_Filename.make_absolute "/sys/fs/cgroup" a in
        let s = Oci_Filename.make_absolute a s in
        s
      in
      move_to_cgroup (abs "cpuset" cgroup);
      move_to_cgroup (abs "cpuacct" cgroup);
      Option.iter ~f:(set_cpuset (abs "cpuset" cgroup)) param.initial_cpuset;
      chmod_cgroup (abs "cpuacct" cgroup)
    end;
    test_userns_availability ();
    (* Option.iter param.rootfs ~f:(mkdir ~perm:0o750); *)
    go_in_userns ~send_pid param.idmaps;
    (* make the mount private and mount basic directories *)
    if param.bind_system_mount then
      mount_base param.rootfs;
    (* chroot in the directory *)
    Unix.chdir param.rootfs;
    (* group must be changed before uid... *)
    setresgid param.rungid param.rungid param.rungid;
    setresuid param.runuid param.runuid param.runuid;
    if not (Sys.file_exists_exn param.command) then begin
      Printf.eprintf "Error: file %s doesn't exists" param.command;
      exit 1
    end;
    let _sessionid = Core.Std.Caml.Unix.setsid () in
    never_returns
      (Unix.exec
         ~prog:param.command
         ~env:(`Replace param.env)
         ~args:(param.command::param.argv) ());
  end ()
