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

open Oci_Client.Git
open Oci_Client.Cmdline

let framac_deps =
  Oci_Client.Cmdline.Predefined.[ocaml;ocamlfind;ocamlgraph;zarith;lablgtk]
let framac_cmds = [
  run "autoconf" [];
  run "./configure" [];
  make ~j:8 [];
  make ["install"];
]
let make_tests j =
  Oci_Client.Git.exec
    "make"
    ~args:[
      (mk_proc "--jobs=%i");
      (mk_proc "PTESTS_OPTS=-error-code -j %i");
      `S "-k";
      `S "tests"]
    ~proc_requested:j

let framac_tests = [
  make ~j:1 ["check-headers"];
  make ~j:8 ["doc"];
  make ~j:1 ["check-logic-parser-wildcard"];
  make_tests 8;
]

let framac = mk_repo
    "frama-c"
    ~url:"git@git.frama-c.com:frama-c/frama-c.git"
    ~deps:framac_deps
    ~cmds:framac_cmds
    ~tests:framac_tests

let plugins = Queue.create ()
let mk_framac_plugin_repo ?revspec ?noconfigure ~url ~deps ~has_tests name=
  let configure =
    match noconfigure with
    | None -> [run "autoconf" []; run "./configure" []]
    | Some () -> []
  in
  let compilation =
    configure @ [
      make ~j:8 [];
      make ["install"];
    ]
  in
  let tests =
    if has_tests
    then [make_tests 4]
    else []
  in
  let plugin = mk_repo
      name
      ?revspec
      ~url
      ~deps:(framac::deps)
      ~cmds:compilation
      ~tests in
  Queue.enqueue plugins (plugin,deps);
  plugin

let _pathcrawler = mk_framac_plugin_repo
    "PathCrawler"
    ~url:"git@git.frama-c.com:pathcrawler/pathcrawler.git"
    ~deps:[Oci_Client.Cmdline.Predefined.xmllight]
    ~has_tests:true

let _genassigns = mk_framac_plugin_repo
    "Genassigns"
    ~url:"git@git.frama-c.com:frama-c/genassigns.git"
    ~deps:[]
    ~has_tests:true

let eacsl = mk_framac_plugin_repo
    "E-ACSL"
    ~url:"git@git.frama-c.com:frama-c/e-acsl.git"
    ~deps:[]
    ~has_tests:true

let _context_from_precondition = mk_framac_plugin_repo
    "context-from-precondition"
    ~url:"git@git.frama-c.com:signoles/context-from-precondition.git"
    ~deps:[eacsl]
    ~has_tests:false

let _a3export = mk_framac_plugin_repo
    "a3export"
    ~url:"git@git.frama-c.com:frama-c/a3export.git"
    ~deps:[]
    ~has_tests:false

let _mthread = mk_framac_plugin_repo
    "Mthread"
    ~url:"git@git.frama-c.com:frama-c/mthread.git"
    ~deps:[]
    ~noconfigure:()
    ~has_tests:true

let _framac_internal =
  let name = "frama-c-internal" in
  let plugins =
    plugins
    |> Queue.to_list
  in
  let plugins_name = String.Set.of_list
      (List.map ~f:(fun (x,_) -> x.name)
         plugins)
  in
  let plugins_deps =
    plugins
    |> List.map ~f:(fun (_,deps) ->
        List.filter deps
          ~f:(fun dep -> not (String.Set.mem plugins_name dep.name)))
    |> List.concat
  in
  let cloneplugins =
    plugins
    |> List.map ~f:(fun (pkg,_) ->
        git_clone ~dir:(Oci_Filename.concat "src/plugins" name) pkg.name
      )
  in
  Oci_Client.Cmdline.add_repo name
    (Oci_Client.Git.repo
       ~deps:
         (List.dedup (List.map ~f:(fun x -> x.name)
                        (framac_deps@plugins_deps)))
       ~cmds:([git_clone framac.url] @
              cloneplugins @
              framac_cmds)
       ~tests:([run "frama-c" ["-plugins"]] @ framac_tests));
  (name,"")

let _framac_external =
  let name = "frama-c-external" in
  Oci_Client.Cmdline.add_repo name
    (Oci_Client.Git.repo
       ~deps:(List.map ~f:(fun (dep,_) -> dep.name) (Queue.to_list plugins))
       ~cmds:[]
       ~tests:[run "frama-c" ["-plugins"]]);
  (name,"")

(**
Actuelle:
 - Tester la doc compile (externe/interne)
 - Tester la doc en externe
 - Faire la distrib OPEN-SOURCE + CLOSE-SOURCE et la compiler,
   la tester + make doc tous faire avec lui
 - example du manuel du developper.
 - example du manuel d'acsl fonctionne et ceux qui ne fonctionne pas

En plus:
 - etude de cas
 - apron
 - qualif
 - bisecter pour trouver la cause.
*)

let () =
  don't_wait_for (default_cmdline ());
  never_returns (Scheduler.go ())
