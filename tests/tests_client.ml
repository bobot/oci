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

(** CI tests *)
open Oci_Client.Git

let oci_sort_url =
  "https://github.com/bobot/oci-repository-for-tutorial.git"

let oci_sort_bak = mk_repo
    "oci-sort_bak"
    ~url:oci_sort_url
    ~deps:Oci_Client.Cmdline.Predefined.[ocaml;ocamlbuild;ocamlfind]
    ~cmds:[
      run "autoconf" [];
      run "./configure" [];
      make [];
      make ["install"];
    ]
    ~tests:[
      make ["tests"];
    ]

let oci_sort_ocamlparam =
  WP.mk_param ~default:None "oci-sort-ocamlparam"
    ~sexp_of:[%sexp_of: string option]
    ~of_sexp:[%of_sexp: string option]
    ~cmdliner:Arg.(value & (opt (some (some string)) None)
                   & info ["oci-sort-ocamlparam"]
                     ~docv:"ARG"
                     ~doc:"Determine the argument to give to ocaml \
                           OCAMLPARAM")
    ~to_option_hum:(function None -> "" | Some s -> "--oci-sort-ocamlparam="^s)
let oci_sort_revspec =
  mk_revspec_param ~url:oci_sort_url "oci-sort"

let oci_sort =
  add_repo_with_param "oci-sort"
    WP.(const (fun commit ocamlparam ->
        Oci_Client.Git.repo
          ~deps:Oci_Client.Cmdline.Predefined.[ocaml;ocamlbuild;
                                               ocamlfind]
          ~cmds:[
            Oci_Client.Git.git_clone ~url:oci_sort_url commit;
            run "autoconf" [];
            run "./configure" [];
            make ?env:(match ocamlparam with
                | None -> None
                | Some v -> Some (`Extend ["OCAMLPARAM", v])) [];
            make ["install"];
          ]
          ~tests:[
            make ["tests"];
          ]
          ())
        $? oci_sort_revspec
        $? oci_sort_ocamlparam);
  "oci-sort"

(** benchmark tests *)

let run_compare_oci_sort y master =
  Oci_Client.Git.repo
    ~save_artefact:false
    ~deps:[oci_sort]
    ~cmds:
      [Oci_Client.Git.git_copy_file ~url:oci_sort_url ~src:y
         ~dst:(Oci_Filename.basename y)
         (Option.value_exn ~here:[%here] master)]
    ~tests:[run
              ~memlimit:(Byte_units.create `Megabytes 500.)
              ~timelimit:(Time.Span.create ~sec:10 ())
              "oci-sort" [Oci_Filename.basename y]]
    ()

let () = mk_compare
    ~x_of_sexp:Oci_Common.Commit.t_of_sexp
    ~sexp_of_x:Oci_Common.Commit.sexp_of_t
    ~y_of_sexp:Oci_Filename.t_of_sexp
    ~sexp_of_y:Oci_Filename.sexp_of_t
    ~repos:(fun conn revspecs x y ->
        let revspecs = WP.ParamValue.set revspecs
            oci_sort_revspec (Oci_Common.Commit.to_string x) in
        commit_of_revspec conn ~url:oci_sort_url ~revspec:"master"
        >>= fun master ->
        return (revspecs, WP.const (run_compare_oci_sort y master)))
    ~analyse:(fun _  timed ->
        Ok (Time.Span.to_sec timed.Oci_Common.Timed.cpu_user))
    "oci-sort"

let () = mk_compare
    ~x_of_sexp:WP.ParamValue.t_of_sexp
    ~sexp_of_x:WP.ParamValue.sexp_of_t
    ~y_of_sexp:Oci_Filename.t_of_sexp
    ~sexp_of_y:Oci_Filename.sexp_of_t
    ~repos:(fun conn revspecs x y ->
        let revspecs = WP.ParamValue.replace_by revspecs x in
        commit_of_revspec conn ~url:oci_sort_url ~revspec:"master"
        >>= fun master ->
        return (revspecs, WP.const (run_compare_oci_sort y master)))
    ~analyse:(fun _  timed ->
        Ok (Time.Span.to_sec timed.Oci_Common.Timed.cpu_user))
    "oci-sort_ocaml"

let () = mk_compare_many_using_revspecs
    ~y_of_sexp:Oci_Filename.t_of_sexp
    ~sexp_of_y:Oci_Filename.sexp_of_t
    ~repos:["oci-sort",
            (fun conn y ->
               commit_of_revspec conn ~url:oci_sort_url ~revspec:"master"
               >>= fun master ->
               return (WP.const (run_compare_oci_sort y master)))]
    ~analyse:(fun _  timed ->
        Ok (Time.Span.to_sec timed.Oci_Common.Timed.cpu_user))
    "sorting"



let () =
  don't_wait_for (Oci_Client.Cmdline.default_cmdline
                    ~cmds_with_connections
                    ~doc:"Oci client for tests"
                    ~version:Oci_Client.oci_version
                    "oci_default_client");
  never_returns (Scheduler.go ())
