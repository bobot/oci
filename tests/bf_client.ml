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

open Log.Global

let absolutize = Oci_Filename.make_absolute (Caml.Sys.getcwd ())

let pp_kind fmt = function
  | Oci_Log.Chapter -> Format.fprintf fmt "P"
  | Oci_Log.Standard -> Format.fprintf fmt "S"
  | Oci_Log.Error -> Format.fprintf fmt "E"
  | Oci_Log.Command -> Format.fprintf fmt "C"

let print_time fmt t =
  if Log.Global.level () = `Debug then Time.pp fmt t
  else Format.pp_print_string fmt (Time.format t "%H:%M:%S")

let exec_one test input ~init ~fold sexp_input sexp_output conn =
  debug "Input %s\n%!" (Sexp.to_string_hum (sexp_input input));
  Rpc.Pipe_rpc.dispatch_exn (Oci_Data.log test) conn input
  >>= fun (p,_) ->
  let stdout = (Lazy.force Writer.stdout) in
  let fmt = Writer.to_formatter stdout in
  let open Textutils.Std in
  Pipe.fold p ~init ~f:(fun acc -> function
      | {Oci_Log.data=Oci_Log.Std (kind,line);time}
        when Console.is_color_tty () ->
        Format.fprintf fmt
          "[%a] %s\n%!"
          print_time time
          (Console.Ansi.string_with_attr
             [Oci_Log.color_of_kind kind]
             line);
        return acc
      | {Oci_Log.data=Oci_Log.Std (kind,line);time} ->
        Format.fprintf fmt
          "[%a: %a] %s@."
          pp_kind kind
          print_time time
          line;
        return acc
      | {Oci_Log.data=Oci_Log.Extra r} ->
        Format.fprintf fmt
          "[Result] %s@."
          (Sexp.to_string_hum ((Or_error.sexp_of_t sexp_output) r));
        return (fold acc r)
    )
  >>= fun res ->
  Writer.flushed stdout
  >>= fun () ->
  return res

let forget test input sexp_input _sexp_output conn =
  debug "Forget %s\n%!" (Sexp.to_string_hum (sexp_input input));
  Rpc.Rpc.dispatch_exn (Oci_Data.forget test) conn input
  >>= fun r ->
  info "%s@."
    (Sexp.to_string_hum ((Or_error.sexp_of_t Unit.sexp_of_t) r));
  Deferred.return `Ok

let exec test ?(init=`Ok)
    ?(fold=fun acc _ -> acc) input sexp_input sexp_output conn =
  if Sys.getenv "OCIFORGET" = None
  then exec_one ~init ~fold test input sexp_input sexp_output conn
  else forget test input sexp_input sexp_output conn

type default_revspec = {
  name: string;
  revspec: string;
}

type copts = { verb : Log.Level.t;  socket: string}

let db_repos = ref String.Map.empty
let url_to_default_revspec = String.Table.create ()

let create_query _ccopt rootfs revspecs repo socket =
  let open Oci_Generic_Masters_Api.CompileGitRepo in
  Rpc.Rpc.dispatch_exn (Oci_Data.rpc Oci_Rootfs_Api.find_rootfs) socket rootfs
  >>= fun rootfs ->
  info "Check the revspecs:";
  let query : Query.t = {
    name = repo;
    rootfs = Or_error.ok_exn rootfs;
    repos = !db_repos;
  } in
  let used_repos = Query.used_repos query in
  let cache = String.Table.create () in
  let commits_cmdline = Buffer.create 100 in
  let get_commit url = String.Table.find_or_add cache url
      ~default:(fun () ->
          let def = String.Table.find_exn url_to_default_revspec url in
          let revspec = String.Map.find_exn revspecs def.name in
          Rpc.Rpc.dispatch_exn
            (Oci_Data.rpc Oci_Generic_Masters_Api.GitRemoteBranch.rpc)
            socket {url;revspec}
          >>= fun r ->
          match Or_error.ok_exn r with
          | None ->
            error "%s correspond to no known ref" def.name; exit 1
          | Some commit ->
            let msg =sprintf " --%s %s"
                def.name (Oci_Common.Commit.to_string commit)
            in
            info "%s: %s" def.name (Oci_Common.Commit.to_string commit);
            Buffer.add_string commits_cmdline msg;
            return commit
        )
  in
  (** replace commit in repos *)
  Deferred.Map.map
    used_repos
    ~how:`Parallel
    ~f:(fun repo ->
        Deferred.List.map
          repo.cmds
          ~f:(function
              | Oci_Generic_Masters_Api.CompileGitRepoRunner.Exec _ as x ->
                return x
              | Oci_Generic_Masters_Api.CompileGitRepoRunner.GitClone x ->
                get_commit x.url
                >>= fun commit ->
                return (Oci_Generic_Masters_Api.CompileGitRepoRunner.GitClone
                          {x with commit})
            )
        >>= fun cmds ->
        return {repo with cmds}
      )
  >>= fun repos ->
  info "commits:%s" (Buffer.contents commits_cmdline);
  return { query with repos}

let run ccopt rootfs revspecs (repo:string) socket =
  create_query ccopt rootfs revspecs repo socket
  >>= fun query ->
  let fold acc = function
    | Result.Ok (`Compilation (`Ok _)) -> acc
    | Result.Ok (`Compilation (`Failed _)) -> `Error
    | Result.Ok (`Test (`Ok,_)) -> acc
    | Result.Ok (`Test (`Failed,_)) -> `Error
    | Result.Error _ -> `Error
  in
  exec Oci_Generic_Masters_Api.CompileGitRepo.rpc query ~fold
    Oci_Generic_Masters_Api.CompileGitRepo.Query.sexp_of_t
    Oci_Generic_Masters_Api.CompileGitRepo.Result.sexp_of_t socket

let xpra ccopt rootfs revspecs repo socket =
  create_query ccopt rootfs revspecs repo socket
  >>= fun query ->
  let query = {
    query with repos =
                 String.Map.change query.repos repo (function
                     | None -> assert false (** absurd: the main repo is used *)
                     | Some data ->
                     Some {data with cmds = List.filter ~f:(function
                           | Oci_Generic_Masters_Api.
                               CompileGitRepoRunner.GitClone _ -> true
                           | Oci_Generic_Masters_Api.
                               CompileGitRepoRunner.Exec _ -> false) data.cmds
                         })
  }
  in
  exec Oci_Generic_Masters_Api.XpraGitRepo.rpc query
    Oci_Generic_Masters_Api.CompileGitRepo.Query.sexp_of_t
    Oci_Generic_Masters_Api.XpraRunner.Result.sexp_of_t socket

let list_rootfs _copts rootfs socket =
  let open Oci_Rootfs_Api in
  Deferred.List.fold rootfs
    ~init:`Ok
    ~f:(fun acc rootfs ->
        exec find_rootfs rootfs
          ~init:acc
          Rootfs_Id.sexp_of_t
          Rootfs.sexp_of_t socket
      )

let add_packages rootfs packages =
  let open Oci_Rootfs_Api in
  exec add_packages
    { id = rootfs;
      packages = packages;
    }
    sexp_of_add_packages_query
    Rootfs.sexp_of_t

let connect ccopt cmd =
  Tcp.connect (Tcp.to_file ccopt.socket)
  >>= fun (_,reader,writer) ->
  Rpc.Connection.create
    ~connection_state:(fun _ -> ())
    reader writer
  >>= fun socket ->
  let socket = Result.ok_exn socket in
  cmd socket
  >>= fun res ->
  Rpc.Connection.close socket
  >>= fun () ->
  return res

let run ccopt rootfs revspecs repo =
  connect ccopt (run ccopt rootfs revspecs repo)

let xpra ccopt rootfs revspecs repo =
  connect ccopt (xpra ccopt rootfs revspecs repo)

let list_rootfs ccopt rootfs =
  connect ccopt (list_rootfs ccopt rootfs)

let add_packages ccopt rootfs packages =
  connect ccopt (add_packages rootfs packages)

open Cmdliner;;

(** Configuration *)

module Configuration = struct

  let run ?(env=`Extend []) cmd args =
    Oci_Generic_Masters_Api.CompileGitRepoRunner.Exec {
      cmd;
      args = List.map args ~f:(fun s -> `S s);
      env;
      proc_requested = 1;
      working_dir = Oci_Filename.current_dir;
    }

  let make ?(j=1) ?(vars=[]) ?(env=`Extend []) targets =
    Oci_Generic_Masters_Api.CompileGitRepoRunner.Exec {
      cmd = "make";
      args =
        `S "-j" :: `Proc ::
        List.map vars ~f:(fun (var,v) -> `S (var^"="^v)) @
        List.map targets ~f:(fun s -> `S s);
      env;
      proc_requested = j;
      working_dir = Oci_Filename.current_dir;
    }

  let dumb_commit = Oci_Common.Commit.of_string_exn (String.make 40 '0')

  let gitclone ?(dir=Oci_Filename.current_dir) url =
    Oci_Generic_Masters_Api.CompileGitRepoRunner.GitClone
      {url;commit=dumb_commit;
       directory=dir}

  let mk_repo ?(revspec="master") ~url ~deps ~cmds ?(tests=[]) name =
    let id = (name,url) in
    let data = {
      Oci_Generic_Masters_Api.CompileGitRepo.Query.deps = List.map ~f:fst deps;
      cmds = (gitclone (snd id))::cmds;
      tests;
    }
    in
    String.Table.add_exn url_to_default_revspec
      ~key:url ~data:{name;revspec};
    db_repos := String.Map.add !db_repos ~key:name ~data;
    id

  let () =
    let ocaml = mk_repo
        "ocaml"
        ~url:"git@git.frama-c.com:bobot/ocaml.git"
        ~revspec:"bdf3b0fac7dd2c93f80475c9f7774b62295860c1"
        ~deps:[]
        ~cmds:[
          run "./configure" [];
          make ["world.opt"];
          make ["install"];
          run "mkdir" ["-p";"/usr/local/lib/ocaml/site-lib/stublibs/"];
          run "touch" ["/usr/local/lib/ocaml/site-lib/stublibs/.placeholder"];
          run "sh" ["-c";"echo /usr/local/lib/ocaml/site-lib/stublibs/ >> \
                          /usr/local/lib/ocaml/ld.conf"]
        ]
    in
    let ocamlfind = mk_repo
        "ocamlfind"
        ~url:"git@git.frama-c.com:bobot/ocamlfind.git"
        ~deps:[ocaml]
        ~cmds:[
          run "./configure" [];
          make ["all"];
          make ["opt"];
          make ["install"];
        ]
    in
    let zarith = mk_repo
        "ZArith"
        ~url:"git@git.frama-c.com:bobot/zarith.git"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          run "./configure" [];
          make [];
          make ["install"];
        ]
    in
    let xmllight = mk_repo
        "xml-light"
        ~url:"https://github.com/ncannasse/xml-light.git"
        ~revspec:"2.4"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          make ["install_ocamlfind"]
        ]
    in
    let camlp4 = mk_repo
        "camlp4"
        ~url:"https://github.com/ocaml/camlp4.git"
        ~revspec:"4.02+6"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          run "./configure" [];
          make ["all"];
          make ["install";"install-META"]
        ]
    in
    let lablgtk = mk_repo
        "lablgtk"
        ~url:"https://forge.ocamlcore.org/anonscm/git/lablgtk/lablgtk.git"
        ~revspec:"28290b0ee79817510bbc908bc733e80258aea7c1"
        ~deps:[ocaml;ocamlfind;camlp4]
        ~cmds:[
          run "./configure" [];
          make ["world"];
          make ~env:(`Extend ["OCAMLFIND_LDCONF","ignore"]) ["install"];
        ] in
    let ocamlgraph = mk_repo
        "ocamlgraph"
        ~url:"https://github.com/backtracking/ocamlgraph.git"
        ~deps:[ocaml;ocamlfind;lablgtk]
        ~cmds:[
          run "autoconf" [];
          run "./configure" [];
          make [];
          make ["install-findlib"];
        ]
    in
    let framac_deps = [ocaml;ocamlfind;ocamlgraph;zarith;lablgtk] in
    let framac_cmds = [
          run "autoconf" [];
          run "./configure" [];
          make ~j:8 [];
          make ["install"];
    ] in
    let framac_tests = [
          make ~j:1
            ~vars:["PTESTS_OPTS","-error-code -j 8"] ["tests"]
    ] in
    let framac = mk_repo
        "frama-c"
        ~url:"git@git.frama-c.com:frama-c/frama-c.git"
        ~deps:framac_deps
        ~cmds:framac_cmds
        ~tests:framac_tests
    in
    let plugins = Queue.create () in
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
        then [make
                ~vars:["PTESTS_OPTS","-error-code -j 4"] ["tests"]]
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
    in
    let _genassigns = mk_framac_plugin_repo
        "Genassigns"
        ~url:"git@git.frama-c.com:frama-c/genassigns.git"
        ~deps:[]
        ~has_tests:true
    in
    let eacsl = mk_framac_plugin_repo
        "E-ACSL"
        ~url:"git@git.frama-c.com:frama-c/e-acsl.git"
        ~deps:[]
        ~has_tests:true
    in
    let _context_from_precondition = mk_framac_plugin_repo
        "context-from-precondition"
        ~url:"git@git.frama-c.com:signoles/context-from-precondition.git"
        ~deps:[eacsl]
        ~has_tests:false
    in
    let _a3export = mk_framac_plugin_repo
        "a3export"
        ~url:"git@git.frama-c.com:frama-c/a3export.git"
        ~deps:[]
        ~has_tests:false
    in
    let _mthread = mk_framac_plugin_repo
        "Mthread"
        ~url:"git@git.frama-c.com:frama-c/mthread.git"
        ~deps:[]
        ~noconfigure:()
        ~has_tests:true
    in
    let _pathcrawler = mk_framac_plugin_repo
        "PathCrawler"
        ~url:"git@git.frama-c.com:pathcrawler/pathcrawler.git"
        ~deps:[xmllight]
        ~has_tests:true
    in
    let _framac_internal =
      let name = "frama-c-internal" in
      let plugins =
        plugins
        |> Queue.to_list
      in
      let plugins_name = String.Set.of_list
          (List.map ~f:(fun ((name,_),_) -> name)
             plugins)
      in
      let plugins_deps =
        plugins
        |> List.map ~f:(fun (_,deps) ->
            List.filter deps
              ~f:(fun (dep,_) -> not (String.Set.mem plugins_name dep)))
        |> List.concat
      in
      let cloneplugins =
        plugins
        |> List.map ~f:(fun ((name,_) as pkg,_) ->
            gitclone ~dir:(Oci_Filename.concat "src/plugins" name) (snd pkg)
          )
      in
      let data = {
        Oci_Generic_Masters_Api.CompileGitRepo.Query.deps =
          List.dedup (List.map ~f:fst (framac_deps@plugins_deps));
        cmds =
          [gitclone (snd framac)] @
          cloneplugins @
          framac_cmds;
        tests =
          framac_tests @
          [run "frama-c" ["-plugins"]];
      }
      in
      db_repos := String.Map.add !db_repos ~key:name ~data;
      (name,"")
    in
    let _framac_external =
      let name = "frama-c-external" in
      let data = {
        Oci_Generic_Masters_Api.CompileGitRepo.Query.deps =
          List.map ~f:(fun ((name,_),_) -> name) (Queue.to_list plugins);
        cmds = [];
        tests = [
          run "frama-c" ["-plugins"];
        ];
      }
      in
      db_repos := String.Map.add !db_repos ~key:name ~data;
      (name,"")
    in
    ()

end

(**
Actuelle:
 - Tester l'interne et la doc compile
 - Tester la doc en externe
 - Tester les entêtes pour frama-c
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

(** Commandline handling *)
(* Help sections common to all commands *)

let copts_sect = "COMMON OPTIONS"
let help_secs = [
 `S copts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `S "BUGS"; `P "Check bug reports at the oci repository.";]


let copts verb socket =
  set_level verb;
  { verb; socket}
let copts_t =
  let docs = copts_sect in
  let verb =
    let doc = "Suppress informational output." in
    let quiet = `Error, Arg.info ["q"; "quiet"] ~docs ~doc in
    let doc = "Give verbose output." in
    let verbose = `Debug, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [`Info] [quiet; verbose])
  in
  let socket =
    Arg.(required & opt (some file) None & info ["socket"]
           ~docv:"FILE"
           ~doc:"specify the pathname of the unix socket of the server"
        )
  in
  Term.(const copts $ verb $ socket)

(* Commands *)

let rootfs_converter =
  (fun s -> try `Ok (Oci_Rootfs_Api.Rootfs_Id.of_string s)
    with _ -> `Error "Rootfs should be a positive number"),
  (fun fmt s ->
     Format.pp_print_string fmt (Oci_Rootfs_Api.Rootfs_Id.to_string s))

let list_rootfs_cmd =
  let rootfs =
    Arg.(value & pos_all rootfs_converter [] & info []
           ~docv:"ID"
           ~doc:"rootfs to get information from.")
  in
  let doc = "list the currently existing rootfs" in
  let man = [
    `S "DESCRIPTION";
    `P "Return all informations about all the existing repositories or \
        the one optionally given as argument"] @ help_secs
  in
  Term.(const list_rootfs $ copts_t $ rootfs),
  Term.info "list-rootfs" ~sdocs:copts_sect ~doc ~man

let add_package_cmd =
  let rootfs =
    Arg.(required & opt (some rootfs_converter) None & info ["rootfs"]
           ~docv:"ID"
           ~doc:"Specify on which rootfs to start")
  in
  let packages =
    Arg.(non_empty & pos_all string [] & info []
           ~docv:"PKG"
           ~doc:"package to add")
  in
  let doc = "Create a new rootfs by adding packages in a rootfs" in
  let man = [
    `S "DESCRIPTION";
    `P "Add the specified packages in the given rootfs"] @ help_secs
  in
  Term.(const add_packages $ copts_t $ rootfs $ packages),
  Term.info "add-package" ~sdocs:copts_sect ~doc ~man

let run_cmd,xpra_cmd =
  let rootfs =
    Arg.(required & opt (some rootfs_converter) None & info ["rootfs"]
           ~docv:"ID"
           ~doc:"Specify on which rootfs to run")
  in
  let revspecs =
    String.Table.fold url_to_default_revspec ~init:Term.(const String.Map.empty)
      ~f:(fun ~key:_ ~data acc ->
            Term.(const (fun revspec acc ->
                String.Map.add ~key:data.name ~data:revspec acc) $
                Arg.(value & opt string data.revspec & info [data.name]
                       ~docv:"REVSPEC"
                       ~doc:(sprintf "indicate which revspec of %s to use."
                               data.name)
                    ) $
                acc)
        )
  in
  let repo =
    let repos = String.Map.keys !db_repos in
    let repo_enum = List.map ~f:(fun x -> (x,x)) repos in
    Arg.(required & pos 0 (some (enum repo_enum)) None & info []
           ~docv:"REPO_NAME"
           ~doc:("Run the repository $(docv). \
                  Possible values: " ^ (String.concat ~sep:", " repos) ^ "."))
  in
  let doc = "run the integration of a repository" in
  let man = [
    `S "DESCRIPTION";
    `P "Run the integration of the given repository with the given rootfs
        using the specified commits."] @ help_secs
  in
  (Term.(const run $ copts_t $ rootfs $ revspecs $ repo),
   Term.info "run" ~doc ~sdocs:copts_sect ~man),
  (Term.(const xpra $ copts_t $ rootfs $ revspecs $ repo),
   Term.info "xpra" ~doc ~sdocs:copts_sect ~man)

let default_cmd =
  let doc = "OCI client for Frama-C and Frama-C plugins" in
  let man = help_secs in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "bf_client" ~version:"0.1" ~sdocs:copts_sect ~doc ~man

let cmds = [list_rootfs_cmd; add_package_cmd; run_cmd; xpra_cmd]

let () =
  don't_wait_for begin
    match Term.eval_choice default_cmd cmds with
    | `Error _ -> exit 1
    | `Ok r -> begin r >>= function
      | `Ok -> Shutdown.exit 0
      | `Error -> Shutdown.exit 1
      end
    | `Help | `Version -> exit 0
  end

let () = never_returns (Scheduler.go ())
