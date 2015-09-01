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

let absolutize = Oci_Filename.make_absolute (Caml.Sys.getcwd ())

let exec test input sexp_input sexp_output conn =
  Printf.printf "Read %s\n%!"
    (Sexp.to_string_hum (sexp_input input));
  Rpc.Pipe_rpc.dispatch_exn (Oci_Data.both test) conn input
  >>= fun (p,_) ->
  Pipe.iter p ~f:(function
      | Oci_Data.Line line ->
        Printf.printf
          "[Log: %s] %s\n%!"
          (Sexp.to_string_hum (Oci_Log.sexp_of_kind line.Oci_Log.kind))
          line.Oci_Log.line;
        Deferred.unit
      | Oci_Data.Result r ->
        Printf.printf
          "[Result] For %s: result %s\n%!"
          (Sexp.to_string_hum (sexp_input input))
          (Sexp.to_string_hum ((Or_error.sexp_of_t sexp_output) r));
        Deferred.unit
    )
  >>= fun () ->
  Writer.flushed (Lazy.force Writer.stdout)

let test =
  match Sys.argv.(2) with
  | "succ" ->
    exec Test_succ.test_succ
      (int_of_string Sys.argv.(3)) Int.sexp_of_t Int.sexp_of_t
  | "fibo" ->
    exec Test_succ.test_fibo
      (int_of_string Sys.argv.(3)) Int.sexp_of_t Int.sexp_of_t
  | "fibo_artefact" ->
    exec Test_succ.test_fibo_artefact
      (int_of_string Sys.argv.(3)) Int.sexp_of_t Int.sexp_of_t
  | "fibo_error_artefact" ->
    exec Test_succ.test_fibo_error_artefact
      (int_of_string Sys.argv.(3))
      Int.sexp_of_t Int.sexp_of_t
  | "rootfs" ->
    let open Oci_Rootfs_Api in
    exec create_rootfs
      { meta_tar = Some (absolutize Sys.argv.(3));
        rootfs_tar = absolutize Sys.argv.(4);
        rootfs_info = { distribution = "debian";
                        release = "jessie";
                        arch = "amd64";
                        packages = [];
                        comment = "";
                      }
      }
      sexp_of_create_rootfs_query
      sexp_of_rootfs
  | "lookup_rootfs" ->
    let open Oci_Rootfs_Api in
    exec find_rootfs (Rootfs_Id.of_string Sys.argv.(3))
      Rootfs_Id.sexp_of_t
      sexp_of_rootfs
  | "add_packages" ->
    let open Oci_Rootfs_Api in
    exec add_packages
      { id = (Rootfs_Id.of_string Sys.argv.(3));
        packages = String.split ~on:',' Sys.argv.(4);
      }
      sexp_of_add_packages_query
      sexp_of_rootfs
  | _ -> failwith "succ or fibo"


let _ =
  Tcp.connect (Tcp.to_file Sys.argv.(1))
  >>= fun (_,reader,writer) ->
  Rpc.Connection.create
    ~connection_state:(fun _ -> ())
    reader writer
  >>= fun conn ->
  let conn = Result.ok_exn conn in
  test conn
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
