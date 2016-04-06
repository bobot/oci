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
open Async.Std
open Log.Global

module Git_Id : Int_intf.S = Int

let permanent_dir = ref ""
let identity_file = ref None
let next_id = ref (Git_Id.of_int_exn (-1))

let db : Git_Id.t Or_error.t Deferred.t String.Table.t = String.Table.create ()
let db_seq = Git_Id.Table.create ()

let get_dir id =
  Oci_Filename.make_absolute !permanent_dir ((Git_Id.to_string id)^".git")

let oci_ssh_sh () =
  Oci_Filename.make_absolute !permanent_dir "oci_ssh.sh"

let create_oci_ssh_sh () =
  let file = oci_ssh_sh () in
  Oci_Std.unlink_no_fail file
  >>= fun () ->
  Writer.open_file file
  >>= fun writer ->
  Writer.write_line writer "#!/bin/bash";
  begin match !identity_file with
    | None -> Writer.write_line writer "exec ssh \"$@\""
    | Some id ->
      Writer.writef writer
        "ssh \
         -o \"StrictHostKeyChecking no\" \
         -o \"UserKnownHostsFile /dev/null\" \
         -F /dev/null -i %s \"$@\"" id
  end;
  Writer.close writer
  >>= fun () ->
  Unix.chmod ~perm:0o500 file

let get_env () =
  `Extend ["GIT_SSH",oci_ssh_sh ()]

let rec get_next_available_id () =
  Git_Id.incr next_id;
  let id = !next_id in
  let dst = get_dir id in
  Sys.file_exists_exn dst
  >>= fun b ->
  if b then get_next_available_id ()
  else return (id,dst)

let get_id url =
  match String.Table.find db url with
  | Some id -> id
  | None ->
    let id = Deferred.create (fun result ->
        get_next_available_id ()
        >>> fun (id,dst) ->
        Monitor.try_with_or_error ~here:[%here] (fun () ->
            Async_shell.run "git"
              ~env:(get_env ())
              ["clone";"--mirror";"--bare";"--quiet";"--";url;dst]
            >>= fun () -> return id)
        >>> fun r ->
        begin match r with
          | Ok _ -> ()
          | Error _ ->
            (** We don't keep the error in the database,
                So the cloning can be tried again later. *)
            String.Table.remove db url
        end;
        Ivar.fill result r;
      ) in
    String.Table.add_exn db ~key:url ~data:id;
    id

(** one operation at a time by repository *)
let lookup_path url (f:Oci_Filename.t -> 'a Deferred.t) : 'a Deferred.t =
  get_id url
  >>= function
  | Error error -> Error.raise error
  | Ok id ->
    let call seq f =
      Throttle.enqueue seq
        (fun src -> Monitor.try_with_or_error ~here:[%here] (fun () -> f src))
      >>= fun res ->
      return (Or_error.ok_exn res)
    in
    match Git_Id.Table.find db_seq id with
    | Some seq -> call seq f
    | None ->
      let seq = Sequencer.create ~continue_on_error:true (get_dir id) in
      Git_Id.Table.add_exn db_seq ~key:id ~data:seq;
      call seq f


let rec chown_not_shared ({Oci_Common.User.uid;gid} as user) src =
  (** This function run as superroot (like all masters),
      so it is root in its usernamespace *)
  Sys.ls_dir src
  >>= fun files ->
  Unix.chown ~uid ~gid src
  >>= fun () ->
  Deferred.List.iter files ~f:(fun file ->
      let src' = Oci_Filename.make_absolute src file in
      Unix.lstat src'
      >>= fun stat ->
      match (stat : Unix.Stats.t) with
      | { uid = 65534 } (* nobody *)
      | { gid = 65534 } (* nogroup *) ->
        return ()
      | {kind = `Directory} -> chown_not_shared user src'
      | {kind = `File; nlink } when nlink >= 2 -> return ()
      | {kind = `File} ->
        Unix.chown ~uid ~gid src'
      | {kind = `Link} -> return ()
      | {kind = `Block|`Socket|`Fifo|`Char } ->
        return ()
    )


let check_commit_availability ~src commit =
  let commit = Oci_Common.Commit.to_string commit in
  Async_shell.test
    "git" ["-C";src;"rev-parse";"--verify";"-q";commit^"^{commit}"]
  >>= fun b -> begin
    if b then return true (* already in the repository *)
    else
      (** update the local cache.
          It is not possible to fetch a particular commit. *)
      Async_shell.run
        ~env:(get_env ())
        "git" ["-C";src;"fetch";"origin"]
      >>= fun () ->
      (** test if the fetch find it *)  (** well fetched *)
      Async_shell.test
        "git" ["-C";src;"rev-parse";"--verify";"-q";commit^"^{commit}"]
  end

let check_commit_availability_exn ~src commit =
  check_commit_availability ~src commit
  >>= fun b ->
  if b then return ()
  else failwithf "Commit number %s not found on server"
      (Oci_Common.Commit.to_string commit) ()


let clone ~user ~url ~dst ~commit =
  lookup_path url
    (fun src ->
       check_commit_availability_exn ~src commit
       >>= fun () ->
       Async_shell.run
         "git" ["clone";"--local";"--no-checkout";
                "--";src;dst]
    )
  >>= fun () ->
  chown_not_shared (Oci_Common.master_user user) dst
  >>= fun () ->
  return ()


let copy_file ~user ~url ~src:file_src ~dst ~commit =
  lookup_path url
    (fun src ->
       check_commit_availability_exn ~src commit
       >>= fun () ->
       let commit = Oci_Common.Commit.to_string commit in
       Process.create
         ~prog:"git" ~args:["-C";src;"show";commit^":"^file_src] ()
       >>= function
       | Error exn -> Error.raise exn
       | Ok git_show ->
         don't_wait_for (Writer.close (Process.stdin git_show));
         don't_wait_for (Reader.drain (Process.stderr git_show));
         Oci_Std.unlink_no_fail dst
         >>= fun () ->
         Writer.open_file dst
         >>= fun dst ->
         Reader.transfer (Process.stdout git_show) (Writer.pipe dst)
         >>= fun () ->
         Unix.waitpid (Process.pid git_show)
         >>= function
         | Result.Ok () -> Deferred.unit
         | Result.Error _ ->
           invalid_argf
             "Copy_file: Can't find %s at commit %s in %s" file_src commit src
             ()
    )
  >>= fun () ->
  let user = Oci_Common.master_user user in
  Unix.chown ~uid:user.uid ~gid:user.gid dst
  >>= fun () ->
  return ()

let read_file ~url ~src:file_src ~commit =
  lookup_path url
    (fun src ->
       check_commit_availability_exn ~src commit
       >>= fun () ->
       let commit = Oci_Common.Commit.to_string commit in
       Process.create
         ~working_dir:src
         ~prog:"git" ~args:["show";commit^":"^file_src] ()
       >>= function
       | Error exn -> Error.raise exn
       | Ok git_show ->
         don't_wait_for (Writer.close (Process.stdin git_show));
         don't_wait_for (Reader.drain (Process.stderr git_show));
         Reader.contents (Process.stdout git_show)
         >>= fun content ->
         Unix.waitpid (Process.pid git_show)
         >>= function
         | Result.Ok () -> return content
         | Result.Error _ ->
           invalid_argf
             "Read_file: Can't find %s at commit %s" file_src commit ()
    )

let merge_base ~url commit1 commit2 =
  lookup_path url
    (fun src ->
       check_commit_availability_exn ~src commit1
       >>= fun () ->
       check_commit_availability_exn ~src commit2
       >>= fun () ->
       let commit1 = Oci_Common.Commit.to_string commit1 in
       let commit2 = Oci_Common.Commit.to_string commit2 in
       Process.create
         ~working_dir:src
         ~prog:"git"
         ~args:["merge-base";commit1;commit2] ()
       >>= function
       | Error exn -> Error.raise exn
       | Ok proc ->
         don't_wait_for (Writer.close (Process.stdin proc));
         don't_wait_for (Reader.drain (Process.stderr proc));
         Reader.read_line (Process.stdout proc)
         >>= fun content ->
         Unix.waitpid (Process.pid proc)
         >>= function
         | Result.Ok () -> 
           (match content with
              | `Eof ->
                invalid_argf "merge-base for %s %s returned empty string"
                  commit1 commit2 ()
              | `Ok content -> return (Oci_Common.Commit.of_string_exn content))
         | Result.Error _ ->
           invalid_argf "Can't find merge-base for %s %s" commit1 commit2 ()
    )

let time_between_fetch = Time.Span.create ~sec:2 ()

let fetch_only_if_old src =
  let fetch_head = Oci_Filename.make_absolute src "FETCH_HEAD" in
  Sys.file_exists_exn fetch_head
  >>= fun b -> begin
    if not b
    then return true (** must fetch *)
    else begin
      Unix.lstat fetch_head
      >>= fun stat ->
      return (Time.is_earlier stat.Unix.Stats.mtime
                ~than:(Time.add (Time.now ()) time_between_fetch))
    end
  end
  >>= fun b -> begin
    if b
    then
      Async_shell.run
        ~env:(get_env ())
        "git" ["-C";src;"fetch";"origin"]
    else return ()
  end


let commit_of_revspec ~url ~revspec =
  lookup_path url
    (fun src ->
       let slow_path src =
         fetch_only_if_old src
         >>= fun () ->
         Async_shell.run_one
           ~working_dir:src
           ~expect:[0;1] (* perhapse it should be expect:[0] and use the
                             none output *)
           "git" ["rev-parse";"--verify";"-q";revspec^"^{commit}"]
         >>= fun s -> return (Option.map ~f:Oci_Common.Commit.of_string_exn s)
       in
       match Oci_Common.Commit.of_string revspec with
       | None -> slow_path src
       | Some  _ as commit ->
         Async_shell.run_one
           ~expect:[0;1]
           "git" ["-C";src;"rev-parse";"--verify";"-q";revspec^"^{commit}"]
         >>= function
         | Some c when c = revspec -> return commit
         | _ -> slow_path src
    )

let commit_of_branch ~url ~branch =
  lookup_path url
    (fun src ->
       fetch_only_if_old src
       >>= fun () ->
       Async_shell.run_one
         ~working_dir:src
         ~expect:[0;128]
         "git" ["show";"--format=%H"; "-s"; branch]
       >>= function
       | None ->
         Log.Global.printf ~level:`Debug "No branch %s in %s" branch src;
         return None
       | Some s ->
         Log.Global.printf ~level:`Debug "Branch %s in %s is at commit %s"
           branch src s;
         return (Some (Oci_Common.Commit.of_string_exn s))
    )


let last_commit_before ~url ~branch ~time =
  lookup_path url
    (fun src ->
       fetch_only_if_old src
       >>= fun () ->
       Async_shell.run_one
         ~working_dir:src
         "git" ["log";"--before";Time.to_string time;
                "--format=%H"; "-n"; "1"; branch; "--"]
       >>= function
       | None -> return None
       | Some "" -> return None
       | Some s -> return (Some (Oci_Common.Commit.of_string_exn s))
    )

let time_of_commit ~url ~commit =
    lookup_path url
      (fun src ->
         check_commit_availability_exn ~src commit
         >>= fun () ->
         let commit = Oci_Common.Commit.to_string commit in
         Async_shell.run_one
           ~working_dir:src
           "git" ["show";"-s";"--format=%cI";commit;"--"]
         >>= function
       | None -> invalid_argf "Can't get date of commit %s." commit ()
       | Some s -> return (Time.of_string s)
    )


let init ~dir ~register_saver ~identity_file:i =
  permanent_dir := dir;
  identity_file := i;
  let data = Oci_Filename.make_absolute dir "data" in
  let module M = struct
    type t = {next_id: Git_Id.t;
              db: (String.t * Git_Id.t) list;
             } [@@deriving bin_io]
  end in
  register_saver
    ~loader:(fun () ->
        create_oci_ssh_sh ()
        >>= fun () ->
        Oci_Std.read_if_exists data M.bin_reader_t
          (fun x ->
             debug "Oci_Git load %i records" (List.length x.db);
             next_id := x.M.next_id;
             String.Table.clear db;
             List.iter
               ~f:(fun (key,data) ->
                   String.Table.add_exn db ~key
                     ~data:(return (Ok data)))
               x.M.db;
             return ())
      )
    ~saver:(fun () ->
        Oci_Std.backup_and_open_file data
        >>= fun writer ->
        let l = String.Table.fold ~init:[]
            ~f:(fun ~key ~data acc ->
                Option.fold (Deferred.peek data)
                  ~init:acc ~f:(fun acc -> function
                      | Error _ -> acc
                      | Ok data -> (key,data)::acc)
              ) db in
        let r = { M.next_id = !next_id; M.db = l; } in
        debug "Oci_Git save %i records" (List.length l);
        Writer.write_bin_prot writer M.bin_writer_t r;
        Writer.close writer
      )
