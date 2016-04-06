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
open Oci_Common
open ExtUnix.Specific
open Oci_Wrapper_Lib


let get_etc_sub_config ~user ~file =
  let l = In_channel.read_lines file in
  match List.find_map l ~f:(fun s ->
      match String.split s ~on:':' with
      | [u;start;len] when String.equal u user ->
        Some (Int.of_string start,Int.of_string len)
      | _ -> None
    ) with
  | Some s -> s
  | None ->
    eprintf "This user doesn't have subuid or subgid configured (cf %s) \n%!"
      file;
    exit 1

let default_env = ["PATH","/usr/local/bin:/usr/bin:/bin"]
let rungid = 0
let runuid = 0

let first_user_mapped =
  let ug = User_and_group.for_this_process_exn () in
  let ustart,ulen =
    get_etc_sub_config ~user:(User_and_group.user ug) ~file:"/etc/subuid" in
  let gstart, glen =
    get_etc_sub_config ~user:(User_and_group.user ug) ~file:"/etc/subgid" in
  if ulen < 1001 || glen < 1001 then begin
    eprintf
      "This user doesn't have enough subuid or \
       subgid configured (1001 needed)\n%!";
    exit 1
  end;
  {User.uid=ustart;gid=gstart}

let idmaps =
  Oci_Wrapper_Api.idmaps
    ~first_user_mapped:first_user_mapped
    ~in_user:master_user
    [Superroot,1;Root,1000;User,1]

let run_inside f =
  if Unix.getuid () = 0 then begin
    Printf.eprintf "This program shouldn't be run as root!\n%!";
    exit 1
  end;
  Unix.handle_unix_error begin fun () ->
    (* remove the process from the group of the process monitor, and
    detach it from the controlling terminal. It allows to manage the
    shutdown nicely *)
    let _sessionid = Core.Std.Caml.Unix.setsid () in
    test_userns_availability ();
    (* Option.iter rootfs ~f:(mkdir ~perm:0o750); *)
    go_in_userns idmaps;
    (* group must be changed before uid... *)
    setresgid rungid rungid rungid;
    setresuid runuid runuid runuid;
    let _sessionid = Core.Std.Caml.Unix.setsid () in
    f ()
  end ()


let exec prog args =
  run_inside (fun () ->
      never_returns
        (Unix.exec
           ~prog
           ~env:(`Replace default_env)
           ~args:(prog::args) ()))

let clean dryrun =
  let prog = "rm" in
  let args = ["-r";"-f";Oci_Version.default_oci_data] in
  if dryrun
  then exec "echo" (prog::args)
  else exec prog args

open Cmdliner

let exec =
  let prog =
    Arg.(value & pos 0 string "bash" & info []
           ~docv:"prog"
           ~doc:"command to execute.")
  in
  let args =
    Arg.(value & pos_right 0 string [] & info []
           ~docv:"arg"
           ~doc:"parameter of the command to execute.")
  in
  let doc = "execute the command with the same right than a master" in
  let man = [
    `S "DESCRIPTION";
    `P "Execute the given command with the given arguments inside the same \
        kind of container that the one used by masters. Mainly for debugging. \
        As usual options placed after a double hyphen (--) could start with an \
        hyphen (-)."]
  in
  Term.(const exec $ prog $ args),
  Term.info "exec" ~doc ~man


let clean =
  let dryrun =
    Arg.(value & flag & info ["n";"dry-run"]
           ~doc:"Create the container and print the command that would be run.")
  in
  let doc = "remove the data created by the master" in
  let man = [
    `S "DESCRIPTION";
    `P "The data created by the master use the subuid of the user. \
        So one can't remove them with just an rm. This command run the command \
        inside a container correctly configured"]
  in
  Term.(const clean $ dryrun),
  Term.info "clean" ~doc ~man


let cmds = [exec;clean]

let default_cmd =
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "oci_master_tools" ~version:Oci_Version.version

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | `Ok () -> exit 0
  | `Help | `Version -> exit 0
