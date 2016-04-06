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

open Oci_Client.Cmdline

open Cmdliner

let cmds_with_connections =
  let test name rpc =
    let arg =
      Arg.(required & pos 0 (some int) None & info []
             ~docv:"i"
             ~doc:("compute the result of "^name^" for the given number"))
    in
    Term.(Term.const
            (fun i ->
               exec rpc i
                 Int.sexp_of_t Format.pp_print_int) $ arg),
    Term.info name
  in
  [
    test "succ" Tests_api.test_succ;
    test "fibo" Tests_api.test_fibo;
    test "fibo_artefact" Tests_api.test_fibo_artefact;
    test "fibo_error_artefact" Tests_api.test_fibo_error_artefact;
    test "collatz" Tests_api.test_collatz;
  ]

let () =
  don't_wait_for (Oci_Client.Cmdline.default_cmdline
                    ~cmds_with_connections
                    ~doc:"Oci client for tests"
                    ~version:Oci_Client.oci_version
                    "oci_default_client");
  never_returns (Scheduler.go ())
