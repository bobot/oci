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


exception Can't_copy_this_file of Oci_Filename.t

open Core_extended

let unlink_no_fail filename =
  try Unix.unlink filename
  with _ -> ()

let rec copydir
    ~hardlink ~prune_file ~prune_user
    ~chown:({User.uid;gid} as chown) src dst =
  (** This function run as superroot (like all masters),
      so it is root in its usernamespace *)
  let dst_exist = Sys.file_exists_exn dst in
  begin if dst_exist
  then ()
  else
    Unix.mkdir dst;
    Unix.chown ~uid ~gid dst;
  end;
  let files = Sys.ls_dir src in
  List.iter files ~f:(fun file ->
      let src' = Oci_Filename.make_absolute src file in
      let dst' = Oci_Filename.make_absolute dst file in
      if List.mem ~equal:Oci_Filename.equal prune_file src'
      then ()
      else begin
        let stat = Unix.lstat src' in
        match (stat: Unix.stats) with
        | { st_uid = 65534 } (* nobody *) | { st_gid = 65534 } (* nogroup *) ->
          ()
        | {st_kind = S_DIR} ->
          copydir ~hardlink ~prune_file ~prune_user ~chown src' dst'
        | {st_kind = (S_REG | S_LNK); st_uid; st_gid}
          when List.mem ~equal:User.equal prune_user
              {User.uid=st_uid;gid=st_gid}
          -> ()
        | {st_kind = S_REG} ->
          unlink_no_fail dst';
          if hardlink
          then Unix.link ~target:src' ~link_name:dst' ()
          else begin
            Shell.run "cp" ["-a";"--";src';dst'];
            Unix.chown ~uid ~gid dst'
          end
        | {st_kind = S_LNK} ->
          unlink_no_fail dst';
          let tgt = Unix.readlink src' in
          Unix.symlink ~src:tgt ~dst:dst';
          assert (not (Oci_Filename.is_relative dst'));
          if hardlink
          then () (** keep superroot uid *)
          else
            ExtUnix.Specific.fchownat
              (ExtUnix.Specific.file_descr_of_int 0) (** dumb *)
              dst' uid gid
              [ExtUnix.Specific.AT_SYMLINK_NOFOLLOW]
        | {st_kind = S_BLK|S_FIFO|S_SOCK|S_CHR } ->
          raise (Can't_copy_this_file src')
      end
    )

let prune_file = ref[]
let prune_user = ref []
let hardlink = ref false
let chown = ref {User.uid= -1;gid= -1}
let src = ref ""
let dst = ref ""

let parse_user s =
  match String.split ~on:':' s with
  | [uid;gid] -> {User.uid = int_of_string uid; gid = int_of_string gid}
  | _ -> raise (Arg.Bad "user format (uid,gid)")

let descr =
  Arg.align [
    "--prune-file",Arg.String (fun s -> prune_file := s::!prune_file),
    "file to prune";
    "--prune-user",Arg.String (fun s ->
        let u = parse_user s in
        prune_user := u::!prune_user),
    "file that are own by this user is pruned";
    "--hardlink",Arg.Set hardlink,
    "To copy or use hardlink";
    "--chown", Arg.String (fun s -> chown := parse_user s) ,
    "chown to apply to created file (not hardlinked one)";
    "--src", Arg.Set_string src,
    "source directory";
    "--dst", Arg.Set_string dst,
    "destination directory";
  ]

let () =
  Caml.Arg.parse descr
    (fun _ -> raise (Arg.Bad "doesn't accept anonymous argument"))
        "copyhard_link";
  copydir
    ~hardlink:(!hardlink)
    ~prune_file:(!prune_file)
    ~prune_user:(!prune_user)
    ~chown:(!chown)
    !src
    !dst
