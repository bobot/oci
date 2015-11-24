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

let get_id url =
  match String.Table.find db url with
  | Some id -> id
  | None ->
    let id = Deferred.create (fun result ->
        Git_Id.incr next_id;
        let id = !next_id in
        let dst = get_dir id in
        Monitor.try_with_or_error (fun () ->
            Async_shell.run "git"
              ~env:(get_env ())
              ["clone";"--mirror";"--bare";"--";url;dst]
            >>= fun () -> return id)
        >>> fun r ->
        Ivar.fill result r
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
        (fun src -> Monitor.try_with_or_error (fun () -> f src))
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
      >>= function
      | { uid = 65534 } (* nobody *) | { gid = 65534 } (* nogroup *) ->
        return ()
      | {kind = `Directory} -> chown_not_shared user src'
      | {kind = `File; nlink } when nlink >= 2 -> return ()
      | {kind = `File} ->
        Unix.chown ~uid ~gid src'
      | {kind = `Link} -> return ()
      | {kind = `Block|`Socket|`Fifo|`Char } ->
        return ()
    )


let clone ~user ~url ~dst ~commit =
  lookup_path url
    (fun src ->
       let commit = Oci_Common.Commit.to_string commit in
       Async_shell.test
         "git" ["-C";src;"rev-parse";"--verify";"-q";commit^"^{commit}"]
       >>= fun b -> begin
         if b then return () (* already in the repository *)
         else
           (** update the local cache.
               It is not possible to fetch a particular commit. *)
           Async_shell.run
             ~env:(get_env ())
             "git" ["-C";src;"fetch";"origin"]
           >>= fun () ->
           Async_shell.test
             "git" ["-C";src;"rev-parse";"--verify";"-q";commit^"^{commit}"]
           >>= fun b ->
           if b then return () (** well fetched *)
           else failwith "Commit number not found on server"
       end
       >>= fun () ->
       Async_shell.run
         "git" ["clone";"--local";"--no-checkout";
                "--";src;dst]
    )
  >>= fun () ->
  chown_not_shared (Oci_Common.master_user user) dst
  >>= fun () ->
  return ()

let get_remote_branch_commit ~url ~revspec =
  lookup_path url
    (fun src ->
       let fetch_head = Oci_Filename.make_absolute src "FETCH_HEAD" in
       Sys.file_exists_exn fetch_head
       >>= fun b -> begin
         if not b
         then return true (** must fetch *)
         else begin
           Unix.lstat fetch_head
           >>= fun stat ->
           return (Time.is_earlier stat.Unix.Stats.mtime
                     ~than:(Time.add
                              (Time.now ())
                              (Time.Span.create ~sec:2 ())))
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
       >>= fun () ->
       Async_shell.run_one
         ~expect:[0;1]
         "git" ["-C";src;"rev-parse";"--verify";"-q";revspec^"^{commit}"]
       >>= fun s -> return (Option.map ~f:Oci_Common.Commit.of_string_exn s)
    )

let init ~dir ~register_saver ~identity_file:i =
  permanent_dir := dir;
  identity_file := i;
  let data = Oci_Filename.make_absolute dir "data" in
  let module M = struct
    type t = {next_id: Git_Id.t;
              db: (String.t * Git_Id.t Or_error.t) list;
             } with bin_io
  end in
  register_saver
    ~loader:(fun () ->
        create_oci_ssh_sh ()
        >>= fun () ->
        Oci_Std.read_if_exists data M.bin_reader_t
          (fun x ->
             next_id := x.M.next_id;
             String.Table.clear db;
             List.iter
               ~f:(fun (key,data) ->
                   match data with (** For testing *)
                   | Error _ -> ()
                   | Ok _ ->
                   String.Table.add_exn db ~key
                     ~data:(return data))
               x.M.db;
             return ())
      )
    ~saver:(fun () ->
        Oci_Std.backup_and_open_file data
        >>= fun writer ->
        let l = String.Table.fold ~init:[]
            ~f:(fun ~key ~data acc ->
                Option.fold (Deferred.peek data)
                  ~init:acc ~f:(fun acc data -> (key,data)::acc)
              ) db in
        let r = { M.next_id = !next_id; M.db = l; } in
        Writer.write_bin_prot writer M.bin_writer_t r;
        Writer.close writer
      )
