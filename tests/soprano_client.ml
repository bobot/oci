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

(** Configuration *)

open Oci_Client.Git
open Oci_Client.Cmdline

let popop = mk_repo
    "popop"
    ~url:"git@git.frama-c.com:soprano/popop.git"
    ~deps:Oci_Client.Cmdline.Predefined.
            [ocaml;ocamlfind;ounit;cryptokit;zarith;ocamlgraph]
    ~cmds:[
      make [];
      run "cp" ["popop.native";"/usr/local/bin/popop"];
    ]
    ~tests:[
      make ["tests"];
    ]

let () = mk_compare_n
    ~deps:[popop]
    ~x_of_sexp:Oci_Common.Commit.t_of_sexp
    ~sexp_of_x:Oci_Common.Commit.sexp_of_t
    ~y_of_sexp:Oci_Filename.t_of_sexp
    ~sexp_of_y:Oci_Filename.sexp_of_t
    ~cmds:(fun conn revspecs x y ->
        let url = "git@git.frama-c.com:soprano/popop.git" in
        commit_of_revspec conn ~url ~revspec:"master"
        >>= fun master ->
        return
          ((String.Map.add revspecs ~key:"popop"
              ~data:(Some (Oci_Common.Commit.to_string x))),
           [Oci_Client.Git.git_copy_file ~url ~src:y
              ~dst:(Oci_Filename.basename y)
              (Option.value_exn ~here:[%here] master)],
           (run "popop" [Oci_Filename.basename y])))
    ~analyse:(fun _  timed ->
        Some (Time.Span.to_sec timed.Oci_Common.Timed.cpu_user))
    "popop_compare_n"

let () =
  don't_wait_for (default_cmdline ());
  never_returns (Scheduler.go ())
