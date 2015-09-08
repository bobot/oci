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

let compile_git_repo_runner
    t (q:Oci_Generic_Masters_Api.CompileGitRepoRunner.query) =
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
             Oci_Runner.run t ~working_dir ~prog:cmd.cmd ~args ()
          )
      ) q.cmds
  >>= fun () ->
  Oci_Runner.create_artefact t
    ~dir:"/"
    ~prune:[working_dir]


let () =
  never_returns begin
    Oci_Runner.start
      ~implementations:[
        Oci_Runner.implement
          Oci_Generic_Masters_Api.CompileGitRepoRunner.rpc
          compile_git_repo_runner;
      ]
  end
