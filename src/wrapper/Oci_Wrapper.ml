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

(** remove the process from the group of the process monitor, and
    detach it from the controlling terminal. It allows to manage the
    shutdown nicely *)
let _sessionid = Core.Std.Caml.Unix.setsid ()
(* let () = setpgid 0 0 *)

let mkdir ?(perm=0o750) dir =
  if not (Sys.file_exists_exn dir) then Unix.mkdir dir ~perm

let mount_inside ~dir ~src ~tgt ?(fstype="") ~flags ?(option="") () =
  let tgt = Filename.concat dir tgt in
  mkdir tgt;
  mount ~source:src ~target:tgt ~fstype flags ~data:option

let mount_base dir =
(*
  mount ~source:dir ~target:dir ~fstype:"" [MS_BIND;MS_PRIVATE;MS_REC] ~data:"";
*)
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

let set_usermap idmaps pid =
  assert (idmaps <> []);
  let call cmd proj =
    (** newuidmap pid uid loweruid count [uid loweruid count [ ... ]] *)
    let argv = List.fold_left ~f:(fun acc idmap ->
        idmap.length_id::(proj idmap.extern_id)::(proj idmap.intern_id)::acc
      ) ~init:[Pid.to_int pid] idmaps in
    let argv = List.rev_map ~f:string_of_int argv in
    Core_extended.Shell.run ~expect:[0] cmd argv in
  call "newuidmap" (fun u -> u.uid);
  call "newgidmap" (fun u -> u.gid)

let do_as_the_child_on_error pid =
  match Unix.waitpid pid with
  | Ok () -> ()
  | Error (`Exit_non_zero i) -> exit i
  | Error (`Signal s) ->
    Signal.send_i s (`Pid (Unix.getpid ())); assert false

let goto_child ~exec_in_parent =
  let fin,fout = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child -> (** child *)
    Unix.close fout;
    ignore (Unix.read fin ~buf:(Bytes.create 1) ~pos:0 ~len:1);
    Unix.close fin
  | `In_the_parent pid ->
    (** execute the command and wait *)
    Unix.close fin;
    (exec_in_parent pid: unit);
    ignore (Unix.write fout ~buf:(Bytes.create 1) ~pos:0 ~len:1);
    Unix.close fout;
    do_as_the_child_on_error pid;
    exit 0

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
      do_as_the_child_on_error pid
    in
    call_in_child

let exec_now_in_child f arg =
  match Unix.fork () with
  | `In_the_child -> (** child *)
    f arg;
    exit 0
  | `In_the_parent pid ->
    do_as_the_child_on_error pid

let just_goto_child () =
  match Unix.fork () with
  | `In_the_child -> (** child *) ()
  | `In_the_parent pid ->
    do_as_the_child_on_error pid;
    exit 0


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

let go_in_userns idmaps =
  (** the usermap can be set only completely outside the namespace, so we
      keep a child for doing that when we have a pid completely inside the
      namespace *)
  let call_set_usermap = exec_in_child (set_usermap idmaps) in
  unshare [ CLONE_NEWNS;
            CLONE_NEWIPC;
            CLONE_NEWPID;
            CLONE_NEWUTS;
            CLONE_NEWUSER;
          ];
  (** only the child will be in the new pid namespace, the parent is in an
      intermediary state not interesting *)
  goto_child ~exec_in_parent:(fun pid ->
      send_pid pid;
      call_set_usermap pid)
  (* Printf.printf "User: %i (%i)\n%!" (Unix.getuid ()) (Unix.geteuid ()); *)
  (* Printf.printf "Pid: %i\n%!" (Unix.getpid ()); *)
  (* Printf.printf "User: %i (%i)\n%!" (Unix.getuid ()) (Unix.geteuid ()); *)

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
    (* Option.iter param.rootfs ~f:(mkdir ~perm:0o750); *)
    go_in_userns param.idmaps;
    (** make the mount private and mount basic directories *)
    if param.bind_system_mount then
      mount_base param.rootfs;
    (** chroot in the directory *)
    Unix.chdir param.rootfs;
    (** group must be changed before uid... *)
    setresgid param.rungid param.rungid param.rungid;
    setresuid param.runuid param.runuid param.runuid;
    if not (Sys.file_exists_exn param.command) then begin
      Printf.eprintf "Error: file %s doesn't exists" param.command;
      exit 1
    end;
    never_returns
      (Unix.exec
         ~prog:param.command
         ~env:(`Replace param.env)
         ~args:(param.command::param.argv) ());
  end ()
