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

open Oci_Simple_Exec_Api

let run = Rpc.Rpc.implement run
    (fun () param -> Deferred.Or_error.try_with ~name:"Oci_Simple_Exec.run"
        (fun () -> Async_shell.run
            ~setuid:param.runas.uid
            ~setgid:param.runas.gid
            param.prog param.args))

let implementations =
  Rpc.Implementations.create_exn
    ~implementations:[run]
    ~on_unknown_rpc:`Raise

let () =
  Tcp.connect (Tcp.to_file Sys.argv.(1))
  >>> fun (_,reader,writer) ->
  Rpc.Connection.create
    ~implementations
    ~connection_state:(fun _ -> ())
    reader writer
  >>> fun _ ->
  ()

let () = never_returns (Scheduler.go ())
