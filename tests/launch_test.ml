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

let test = match Sys.argv.(2) with
  | "succ" -> Test_succ.test_succ
  | "fibo" -> Test_succ.test_fibo
  | "fibo_artefact" -> Test_succ.test_fibo_artefact
  | "fibo_error_artefact" -> Test_succ.test_fibo_error_artefact
  | _ -> failwith "succ or fibo"


let exec_f conn line =
  Printf.printf "Read %s\n%!" line;
  Rpc.Rpc.dispatch_exn (Oci_Data.rpc test) conn
    (int_of_string line)
  >>= fun r ->
  Printf.printf
    "For %s: result %i\n%!" line r;
  Writer.flushed (Lazy.force Writer.stdout)

let _ =
  Tcp.connect (Tcp.to_file Sys.argv.(1))
  >>= fun (_,reader,writer) ->
  Rpc.Connection.create
    ~connection_state:(fun _ -> ())
    reader writer
  >>= fun conn ->
  let conn = Result.ok_exn conn in
  exec_f conn Sys.argv.(3)
  >>= fun () ->
  Rpc.Connection.close conn;
  >>= fun () ->
  Shutdown.exit 0


  (* begin *)
  (*   (Lazy.force Reader.stdin) *)
  (*   |> Reader.lines *)
  (*   |> Pipe.iter *)
  (*     ~f:(exec_f conn) *)
  (* end *)

let () = never_returns (Scheduler.go ())
