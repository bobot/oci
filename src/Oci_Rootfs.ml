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

open Oci_Rootfs_Api

(** The rootfs master is special because it create the environnement,
    so it need to runs in masters task that should be done in a runner *)

let rootfs_next_id = ref (-1)
let db_rootfs : Rootfs.t Rootfs_Id.Table.t ref = ref (Rootfs_Id.Table.create ())

let testdir () =
  Oci_Master.permanent_directory Oci_Rootfs_Api.create_rootfs
  >>= fun dir ->
  return (Oci_Filename.make_absolute dir "testdir")


let () =
  let module M = struct
    type t = {rootfs_next_id: Int.t;
              db_rootfs: (Rootfs_Id.t * Rootfs.t) list;
             } [@@deriving bin_io]
  end in
  Oci_Master.simple_register_saver
    Oci_Rootfs_Api.create_rootfs
    M.bin_t
    ~basename:"rootfs_next_id"
    ~saver:(fun () ->
        let db_rootfs = Rootfs_Id.Table.to_alist !db_rootfs in
        return {M.rootfs_next_id = !rootfs_next_id;
                db_rootfs})
    ~loader:(fun r ->
        rootfs_next_id := r.M.rootfs_next_id;
        db_rootfs := Rootfs_Id.Table.of_alist_exn r.M.db_rootfs;
        return ())
    ~init:(fun () ->
        testdir ()
        >>= fun testdir ->
        Async_shell.run "rm" ["-rf";"--";testdir]
        >>= fun () ->
        Unix.mkdir ~p:() testdir)

let create_new_rootfs rootfs_query =
  testdir ()
  >>= fun testdir ->
  incr rootfs_next_id;
  let id = Rootfs_Id.of_int_exn (!rootfs_next_id) in
  let testdir = Oci_Filename.make_absolute testdir (Rootfs_Id.to_string id) in
  Monitor.protect ~here:[%here]
    ~finally:(fun () -> Async_shell.run "rm" ["-rf";"--";testdir])
    (fun () ->
       Unix.mkdir testdir
       >>= fun () -> begin
       match rootfs_query.meta_tar with
       | None -> return None
       | Some meta_tar ->
         Oci_Master.cha_log "Extract meta archive: %s" meta_tar;
         let metadir = Oci_Filename.make_absolute testdir "meta" in
         Unix.mkdir metadir
         >>= fun () ->
         Async_shell.run "tar" ["Jxf";meta_tar;"-C";metadir]
         >>= fun () ->
         let exclude = Oci_Filename.make_absolute metadir "excludes-user" in
         Sys.file_exists_exn exclude
         >>= fun exi ->
         if exi
         then return (Some exclude)
         else return None
       end
       >>= fun exclude ->
       Oci_Master.cha_log
         "Extract rootfs archive: %s" rootfs_query.rootfs_tar;
       let rootfsdir = Oci_Filename.make_absolute testdir "rootfs" in
       Unix.mkdir rootfsdir
       >>= fun () ->
       Async_shell.run "tar" (["xf";rootfs_query.rootfs_tar; "--xz";
                               "-C";rootfsdir;
                               "--preserve-order";
                               "--no-same-owner";
                              ]@
                              (match exclude with
                               | None -> []
                               | Some exclude -> ["--exclude-from";exclude]
                              ))
       >>= fun () ->
       Oci_Master.cha_log "Create artefact";
       Oci_Artefact.create
         ~prune:[
           Oci_Filename.make_absolute rootfsdir "dev";
           Oci_Filename.make_absolute rootfsdir "proc";
           Oci_Filename.make_absolute rootfsdir "sys";
           Oci_Filename.make_absolute rootfsdir "run";
         ]
         ~only_new:false
         ~rooted_at:rootfsdir
         ~src:rootfsdir
       >>= fun a ->
       let rootfs = {
         Rootfs.id;
         info = rootfs_query.rootfs_info;
         rootfs = a
       } in
       Rootfs_Id.Table.add_exn !db_rootfs ~key:id ~data:rootfs;
       Oci_Master.cha_log "New rootfs created";
       return rootfs
    )
  >>= fun s ->
  Deferred.Or_error.return s

let find_rootfs key =
  Deferred.Or_error.return (Rootfs_Id.Table.find_exn !db_rootfs key)

let add_packages (d:add_packages_query) =
  let rootfs = Rootfs_Id.Table.find_exn !db_rootfs d.id in
  Oci_Master.start_runner
    ~debug_info:"add packages"
    ~binary_name:"Oci_Cmd_Runner"
  >>= fun (err,runner) ->
  choose [
    choice (err >>= function
      | Ok () -> never ()
      | Error _ as s -> return s) (fun x -> x);
    choice begin
      Monitor.protect ~here:[%here]
        ~finally:(fun () -> Oci_Master.stop_runner runner)
        ~name:"add_packages"
        (fun () ->
           Oci_Master.cha_log "Runner started";
           Oci_Master.dispatch_runner_exn
             Oci_Cmd_Runner_Api.copy_to runner {
             user=Oci_Common.Root;
             artefact=rootfs.rootfs;
             dst="/";
           }
           >>= fun () ->
           Oci_Master.dispatch_runner_exn
             Oci_Cmd_Runner_Api.get_internet runner ()
           >>= fun () ->
           Oci_Master.cha_log "Update Apt Database";
           Oci_Master.dispatch_runner_exn
             Oci_Cmd_Runner_Api.run runner {
             prog = "apt-get";
             args = ["update";
                     (* We disable privilege dropping because it work not well
                         with the current hardlink overlay technique.
                        And we are already sandboxed. *)
                     "--option";"APT::Sandbox::User=root";
                     "--option";"Acquire::Retries=3";
                    ];
             env = `Extend [];
             runas = Root;
           }
           >>= fun () ->
           Oci_Master.cha_log "Install Package";
           Oci_Master.dispatch_runner_exn
             Oci_Cmd_Runner_Api.run runner {
             prog = "apt-get";
             args = "install"::
                    "--yes"::
                    "--option"::"Apt::Install-Recommends=false"::
                    "--option"::"APT::Sandbox::User=root"::
                    "--option"::"Acquire::Retries=3"::
                    d.packages;
             env = `Extend ["DEBIAN_FRONTEND","noninteractive"];
             runas = Root;
           }
           >>= fun () ->
           Oci_Master.cha_log "Clean Package Data";
           Oci_Master.dispatch_runner_exn
             Oci_Cmd_Runner_Api.run runner {
             prog = "apt-get";
             args = ["clean";
                    "--option";"APT::Sandbox::User=root"];
             env = `Extend ["DEBIAN_FRONTEND","noninteractive"];
             runas = Root;
           }
           >>= fun () ->
           Oci_Master.dispatch_runner_exn
             Oci_Cmd_Runner_Api.create_artefact runner "/"
           >>= fun artefact ->
           incr rootfs_next_id;
           let id = Rootfs_Id.of_int_exn (!rootfs_next_id) in
           let rootfs =  {
             Rootfs.id;
             info =
               {rootfs.info with packages = d.packages @ rootfs.info.packages};
             rootfs = artefact;
           } in
           Rootfs_Id.Table.add_exn !db_rootfs ~key:id ~data:rootfs;
           Oci_Master.cha_log "New rootfs created";
           Deferred.Or_error.return rootfs
        )
    end Fn.id]

let init () =
  let register d f =
    Oci_Master.register d
      (fun s -> Oci_Log.init_writer (fun log ->
           Deferred.Or_error.try_with_join
             (fun () -> Oci_Master.attach_log log
                 (fun () -> f s))
           >>= fun res ->
           Oci_Log.write_and_close log res
         ))
  in
  register Oci_Rootfs_Api.create_rootfs create_new_rootfs;
  register Oci_Rootfs_Api.find_rootfs find_rootfs;
  register Oci_Rootfs_Api.add_packages add_packages
