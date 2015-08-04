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

(** execute a program in a new usernamespace *)

(** We can't use Async since we must play with forks and async doesn't
    like that *)
open Core.Std
open ExtUnix.Specific

let mkdir ?(perm=0o750) dir =
  if not (Sys.file_exists_exn dir) then Unix.mkdir dir ~perm

let mount_inside ~dir ~src ~tgt ?(fstype="") ~flags ?(option="") () =
  let tgt = Filename.concat dir tgt in
  mkdir tgt;
  mount ~source:src ~target:tgt ~fstype flags ~data:option

let mount_base dir =
  mount ~source:dir ~target:dir ~fstype:"" [MS_BIND;MS_PRIVATE;MS_REC] ~data:"";
  mount_inside ~dir ~src:"proc" ~tgt:"proc" ~fstype:"proc"
    ~flags:[MS_NOSUID; MS_NOEXEC; MS_NODEV] ();
  mount_inside ~dir ~src:"/sys" ~tgt:"sys" ~flags:[MS_BIND; MS_REC] ();
  mount_inside ~dir ~src:"/dev" ~tgt:"dev" ~flags:[MS_BIND; MS_REC] ();

  mount_inside ~dir ~src:"tmpfs" ~tgt:"dev/shm" ~fstype:"tmpfs"
    ~flags:[MS_NOSUID; MS_STRICTATIME; MS_NODEV]
    ~option:"mode=1777" ();

  mount_inside ~dir ~src:"tmpfs" ~tgt:"run" ~fstype:"tmpfs"
    ~flags:[MS_NOSUID; MS_STRICTATIME; MS_NODEV]
    ~option:"mode=755" ();

  (** for aptitude *)
  mkdir (Filename.concat dir "/run/lock")

let do_chroot dest =
  Sys.chdir dest;
  chroot ".";
  Sys.chdir "/"

let read_in_file fmt =
  Printf.ksprintf (fun file ->
      let c = open_in file in
      let v = input_line c in
      In_channel.close c;
      v
    ) fmt


let test_userns_availability () =
  let unpriviledge_userns_clone =
    "/proc/sys/kernel/unprivileged_userns_clone" in
  if Sys.file_exists_exn unpriviledge_userns_clone then begin
    let v = read_in_file "%s" unpriviledge_userns_clone in
    if v <> "1" then begin
      Printf.eprintf "This kernel is configured to disable unpriviledge user\
                      namespace: %s must be 1\n" unpriviledge_userns_clone;
      exit 1
    end
  end

let write_in_file fmt =
  Printf.ksprintf (fun file ->
      Printf.ksprintf (fun towrite ->
          try
            let cout = open_out file in
            output_string cout towrite;
            Out_channel.close cout
          with _ ->
            Printf.eprintf "Error during write of %s in %s\n"
              towrite file;
            exit 1
        )
    ) fmt

let command fmt = Printf.ksprintf (fun cmd -> Sys.command cmd = 0) fmt

let command_no_fail ?(error=(fun () -> ())) fmt =
  Printf.ksprintf (fun cmd ->
      let c = Sys.command cmd in
      if c <> 0 then begin
        Printf.eprintf "Error during: %s\n%!" cmd;
        error ();
        exit 1;
      end
    ) fmt

(** {2 User namespace} *)
open Oci_Wrapper_Api

let set_usermap uidmap gidmap pid =
  let call cmd idmap =
    (** newuidmap pid uid loweruid count [uid loweruid count [ ... ]] *)
    assert (idmap <> []);
    let argv = List.fold_left ~f:(fun acc idmap ->
        idmap.length_id::idmap.extern_id::idmap.intern_id::acc
      ) ~init:[Pid.to_int pid] idmap in
    let argv = List.rev_map ~f:string_of_int argv in
    Core_extended.Shell.run ~expect:[0] cmd argv in
  call "newuidmap" uidmap;
  call "newgidmap" gidmap
  (*   command_no_fail ~error *)
  (*   "newuidmap %i 0 %i 1 1 %i %i" pid curr_uid id rangeid; *)
  (* command_no_fail ~error *)
  (*   "newgidmap %i 0 %i 1 1 %i %i" pid curr_gid id rangeid *)

let do_as_the_child pid =
  match Unix.waitpid pid with
  | Ok () -> exit 0
  | Error (`Exit_non_zero i) -> exit i
  | Error (`Signal s) -> Signal.send_i s (`Pid (Unix.getpid ())); assert false

let goto_child ~exec_in_parent =
  let fin,fout = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child -> (** child *)
    Unix.close fout;
    ignore (Unix.read fin ~buf:(Bytes.create 1) ~pos:0 ~len:1);
    Unix.close fin
  | `In_the_parent pid ->
    Unix.close fin;
    (exec_in_parent pid: unit);
    ignore (Unix.write fout ~buf:(Bytes.create 1) ~pos:0 ~len:1);
    Unix.close fout;
    do_as_the_child pid

let exec_in_child (type a) f =
  let fin,fout = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child -> (** child *)
    Unix.close fout;
    let cin = Unix.in_channel_of_descr fin in
    let arg = (Marshal.from_channel cin : a) in
    In_channel.close cin;
    f arg;
    exit 0
  | `In_the_parent pid ->
    Unix.close fin;
    let cout = Unix.out_channel_of_descr fout in
    let call_in_child (arg:a) =
      Marshal.to_channel cout arg [];
      Out_channel.close cout;
      do_as_the_child pid
    in
    call_in_child

let exec_now_in_child f arg =
  match Unix.fork () with
  | `In_the_child -> (** child *)
    f arg;
    exit 0
  | `In_the_parent pid ->
    do_as_the_child pid

let just_goto_child () =
  match Unix.fork () with
  | `In_the_child -> (** child *) ()
  | `In_the_parent pid ->
    do_as_the_child pid

let go_in_userns uidmap gidmap =
  (** the usermap can be set only completely outside the namespace, so we
      keep a child for doing that when we have a pid completely inside the
      namespace *)
  let call_set_usermap = exec_in_child (set_usermap uidmap gidmap) in
  unshare [ CLONE_NEWNS;
            CLONE_NEWIPC;
            CLONE_NEWPID;
            CLONE_NEWUTS;
            CLONE_NEWUSER;
          ];
  (** only the child will be in the new pid namespace, the parent is in an
      intermediary state not interesting *)
  goto_child ~exec_in_parent:call_set_usermap
  (* Printf.printf "User: %i (%i)\n%!" (Unix.getuid ()) (Unix.geteuid ()); *)
  (* Printf.printf "Pid: %i\n%!" (Unix.getpid ()); *)
  (* Printf.printf "User: %i (%i)\n%!" (Unix.getuid ()) (Unix.geteuid ()); *)

(* let param = *)
(*   let curr_uid = Unix.getuid () in *)
(*   let curr_gid = Unix.getgid () in *)
(*   let open Arg in *)
(*   let param = ref { *)
(*       rootfs = "rootfs"; *)
(*       uidmap = [{extern_id=curr_uid; intern_id=0;length_id=1}]; *)
(*       gidmap = [{extern_id=curr_gid; intern_id=0;length_id=1}]; *)
(*       command = "/usr/bin/bash"; *)
(*       argv = [||]; *)
(*       env = [||]; *)
(*       uid = 0; *)
(*       gid = 0; *)
(*       bind_system_mount = true; *)
(*     } in *)
(*   parse (align ~limit:80 [ *)
(*       "--dir", *)
(*       String (fun rootfs -> param := {!param with rootfs}), *)
(*       "dir ​directory to use as the root directory"; *)
(*       "--idmap", *)
(*       Tuple [Set_int idmap_id;Set_int idmap_rangeid], *)
(*       "internal_external_range maps additionally uid/gid *)
(*          [internal;internal+range] to [external;external+range]\n\t\ *)
(*        you need a configured /etc/subuid (man subuid)"; *)
(*       "--uid", *)
(*       Set_int setuid, *)
(*       "uid Execute the command as this uid inside the user namespace"; *)
(*       "--gid", *)
(*       Set_int setgid, *)
(*       "gid Execute the command as this gid inside the user namespace"; *)
(*       "--arch", *)
(*       Set_string arch, *)
(*       "arch Specify the architecture of the image \ *)
(*        (eg. amd64, i386, armel,armhf)"; *)
(*       "--distr", *)
(*       Tuple [Set_string distr; Set_string release], *)
(*       "distr_release Specify the distribution and release of the image \ *)
(*        (eg. centos 6, debian jessie, ubuntu precise, gentoo current)"; *)
(*       "--", *)
(*       Rest (fun s -> command := s::!command), *)
(*       "Instead of running /bin/bash in the usernamespace,
         run the given command" *)
(*     ]) *)
(*     (fun _ -> raise (Bad "no anonymous option")) *)
(*     "Test for user-namespace: you need linux at least 3.18. \ *)
(*      In the user-namespace the\n\ *)
(*      current user is root.
       Use LXC download template facilities for getting\n\ *)
(*      the root filesystem."; *)
(*   let idmap = *)
(*     match !idmap_id, !idmap_rangeid with *)
(*     | (-1), _ | _, (-1) -> KeepUser *)
(*     | id, range -> IdMap(id,range) *)
(*   in *)
(*   let command = *)
(*     match List.rev !command with *)
(*     | [] -> "/bin/bash",[|"bash"|] *)
(*     | (cmd::_) as l -> cmd, Array.of_list l in *)
(*   !param *)

let param : parameters = Bin_prot.Utils.bin_read_stream
    ~read:(fun buf ~pos ~len ->
        Bigstring.really_input ~pos ~len In_channel.stdin buf)
    bin_reader_parameters

let test_overlay () =
  (** for test *)
  let test = "/overlay" in
  let ro = Filename.concat test "ro" in
  let rw = Filename.concat test "rw" in
  let wd = Filename.concat test "wd" in
  let ov = Filename.concat test "ov" in
  mkdir test; mkdir ro; mkdir rw; mkdir wd; mkdir ov;
  mount ~source:"overlay" ~target:ov ~fstype:"overlay"
  []
  ~data:(Printf.sprintf "lowerdir=%s,upperdir=%s,workdir=%s" ro rw wd)

let () =
  if Unix.getuid () = 0 then begin
    Printf.eprintf "This program shouldn't be run as root!\n%!";
    exit 1
  end;
  Unix.handle_unix_error begin fun () ->
    test_userns_availability ();
    mkdir ~perm:0o750 param.rootfs;
    go_in_userns param.uidmap param.gidmap;
    command_no_fail "cp /etc/resolv.conf %S"
      (Oci_Filename.concat param.rootfs "etc/resolv.conf");
    (** make the mount private and mount basic directories *)
    mount_base param.rootfs;
    (** chroot in the directory *)
    do_chroot param.rootfs;
    (** group must be changed before uid... *)
    setresgid param.gid param.gid param.gid;
    setresuid param.uid param.uid param.uid;
    never_returns
      (Unix.exec
         ~prog:param.command
         ~env:(`Replace param.env)
         ~args:param.argv ())
  end ()
