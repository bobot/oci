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

let exec_one test input sexp_input sexp_output conn =
  debug "Input %s\n%!" (Sexp.to_string_hum (sexp_input input));
  Rpc.Pipe_rpc.dispatch_exn (Oci_Data.both test) conn input
  >>= fun (p,_) ->
  let open Textutils.Std in
  Pipe.iter p ~f:(function
      | Oci_Data.Line line when Console.is_color_tty () ->
        Format.printf
          "[%a] %s\n%!"
          Time.pp line.Oci_Log.time
          (Console.Ansi.string_with_attr
             [Oci_Log.color_of_kind line.Oci_Log.kind]
             line.Oci_Log.line);
        Deferred.unit
      | Oci_Data.Line line ->
        Format.printf
          "[%a: %a] %s@."
          pp_kind line.Oci_Log.kind
          Time.pp line.Oci_Log.time
          line.Oci_Log.line;
        Deferred.unit
      | Oci_Data.Result r ->
        debug "[Result] For %s@."
          (Sexp.to_string_hum (sexp_input input));
        info "result %s"
          (Sexp.to_string_hum ((Or_error.sexp_of_t sexp_output) r));
        Deferred.unit
    )
  >>= fun () ->
  Writer.flushed (Lazy.force Writer.stdout)

let forget test input sexp_input _sexp_output conn =
  debug "Forget %s\n%!" (Sexp.to_string_hum (sexp_input input));
  Rpc.Rpc.dispatch_exn (Oci_Data.forget test) conn input
  >>= fun r ->
  info "%s@."
    (Sexp.to_string_hum ((Or_error.sexp_of_t Unit.sexp_of_t) r));
  Deferred.unit

let exec test input sexp_input sexp_output conn =
  if Sys.getenv "OCIFORGET" = None
  then exec_one test input sexp_input sexp_output conn
  else forget test input sexp_input sexp_output conn

let run ?(kind=`Required) ?(env=`Extend []) cmd args
  : Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd = {
  cmd;
  args = List.map args ~f:(fun s -> `S s);
  env;
  proc_requested = 1;
  kind;
}

let make ?(j=1) ?(vars=[]) ?(kind=`Required) ?(env=`Extend []) targets :
  Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd = {
  cmd = "make";
  args =
    `S "-j" :: `Proc ::
    List.map vars ~f:(fun (var,v) -> `S (var^"="^v)) @
    List.map targets ~f:(fun s -> `S s);
  env;
  proc_requested = j;
  kind;
}

type repo = {
  commit: string; (** here branch name are accepted *)
  name: string;
  repo: Oci_Generic_Masters_Api.CompileGitRepo.Query.repo;
}

let db_repos = ref String.Map.empty

let dumb_commit = Oci_Common.Commit.of_string_exn (String.make 40 '0')

let mk_repo ?(commit="master") ~url ~deps ~cmds name =
  let open Oci_Generic_Masters_Api.CompileGitRepo in
  let data =
    { commit;
      name;
      repo = {
        Query.url;
        commit = dumb_commit; (** dumb *)
        deps = List.map ~f:(fun x -> x.name) deps;
        cmds;
      }
    }
  in
  db_repos := String.Map.add !db_repos ~key:name ~data;
  data

let () =
  let ocaml = mk_repo
    "ocaml"
    ~url:"git@git.frama-c.com:bobot/ocaml.git"
    ~commit:"bdf3b0fac7dd2c93f80475c9f7774b62295860c1"
    ~deps:[]
    ~cmds:[
      run "./configure" [];
      make ["world.opt"];
      make ["install"];
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
  let ocamlgraph = mk_repo
    "ocamlgraph"
    ~url:"https://github.com/backtracking/ocamlgraph.git"
    ~deps:[ocaml;ocamlfind]
    ~cmds:[
      run "autoconf" [];
      run "./configure" [];
      make [];
      make ["install-findlib"];
    ]
  in
  let zarith = mk_repo
    "ZArith"
    ~url:"git@git.frama-c.com:bobot/zarith.git"
    ~deps:[ocaml;ocamlfind]
    ~cmds:[
      run "./configure" [];
      make [];
      make ~env:(`Extend ["OCAMLFIND_LDCONF","ignore"]) ["install"];
    ]
  in
  let framac = mk_repo
      "frama-c"
      ~url:"git@git.frama-c.com:frama-c/frama-c.git"
      ~deps:[ocaml;ocamlfind;ocamlgraph;zarith]
      ~cmds:[
        run "autoconf" [];
        run "./configure" [];
        make ~j:8 [];
        make ["install"];
        make ~j:1 ~kind:`Test ~vars:["PTESTS_OPTS","-error-code -j 8"] ["tests"]
      ]
  in
  let mk_framac_plugin_repo ?commit ~url ~deps ~has_tests name=
    let compilation = [
      run "autoconf" [];
      run "./configure" [];
      make [];
      make ["install"];
    ]
    in
    let tests =
      if has_tests
      then [make ~kind:`Test ~vars:["PTESTS_OPTS","-error-code -j 4"] ["tests"]]
      else []
    in
    mk_repo
      name
      ?commit
      ~url
      ~deps:(framac::deps)
      ~cmds:(compilation@tests)
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
      ~has_tests:true
  in
  let _pathcrawler = mk_framac_plugin_repo
      "PathCrawler"
      ~url:"git@git.frama-c.com:frama-c/mthread.git"
      ~deps:[]
      ~has_tests:true
  in
  ()


(* Options common to all commands *)
type copts = { verb : Log.Level.t;  socket: string}

let run _ccopt rootfs refspecs repo socket =
  let open Oci_Generic_Masters_Api.CompileGitRepo in
  Rpc.Rpc.dispatch_exn (Oci_Data.rpc Oci_Rootfs_Api.find_rootfs) socket
    (Oci_Rootfs_Api.Rootfs_Id.of_int_exn rootfs)
  >>= fun rootfs ->
  info "Check the refspecs:";
  let query : Query.t = {
    name = repo;
    rootfs = Or_error.ok_exn rootfs;
    repos = String.Map.map ~f:(fun x -> x.repo) !db_repos;
  } in
  let used_repo = Query.used_repos query in
  Deferred.Map.merge
    used_repo refspecs
    ~how:`Parallel
    ~f:(fun ~key:name -> function
        | `Left _ | `Right _ -> return None
        | `Both (repo,refspec) ->
          Rpc.Rpc.dispatch_exn
            (Oci_Data.rpc Oci_Generic_Masters_Api.GitRemoteBranch.rpc)
            socket
            {url = repo.url; refspec}
          >>= fun r ->
          match Or_error.ok_exn r with
          | None ->
            error "%s correspond to no known ref" name; exit 1
          | Some commit ->
            info "--%s %s" name (Oci_Common.Commit.to_string commit);
            return (Some {repo with commit = commit})
      )
  >>= fun repos ->
  exec Oci_Generic_Masters_Api.CompileGitRepo.rpc { query with repos}
    Oci_Generic_Masters_Api.CompileGitRepo.Query.sexp_of_t
    Oci_Generic_Masters_Api.CompileGitRepo.Result.sexp_of_t socket

let list_rootfs _copts _rootfs  = assert false (** TODO *)

let connect ccopt cmd =
  Tcp.connect (Tcp.to_file ccopt.socket)
  >>= fun (_,reader,writer) ->
  Rpc.Connection.create
    ~connection_state:(fun _ -> ())
    reader writer
  >>= fun socket ->
  let socket = Result.ok_exn socket in
  cmd socket
  >>= fun () ->
  Rpc.Connection.close socket;
  >>= fun () ->
  Shutdown.exit 0

let run ccopt rootfs refspecs repo =
  connect ccopt (run ccopt rootfs refspecs repo)

let list_rootfs ccopt rootfs =
  connect ccopt (list_rootfs ccopt rootfs)

open Cmdliner;;

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

let list_rootfs_cmd =
  let repodir =
    Arg.(value & pos_all string [] & info []
           ~docv:"ID"
           ~doc:"rootfs to get information from.")
  in
  let doc = "list the currently existing rootfs" in
  let man = [
    `S "DESCRIPTION";
    `P "Return all informations about all the existing repositories or \
        the one optionally given as argument"] @ help_secs
  in
  Term.(const list_rootfs $ copts_t $ repodir),
  Term.info "list-rootfs" ~sdocs:copts_sect ~doc ~man

let run_cmd =
  let rootfs =
    Arg.(required & opt (some int) None & info ["rootfs"]
           ~docv:"ID"
           ~doc:"Specify on which rootfs to run")
  in
  let refspecs =
    String.Map.fold !db_repos ~init:Term.(const String.Map.empty)
      ~f:(fun ~key ~data acc ->
          Term.(const (fun c acc -> String.Map.add ~key ~data:c acc) $
                Arg.(value & opt string data.commit & info [key]
                       ~docv:"REVSPEC"
                       ~doc:(sprintf "indicate which commit of %s to use. \
                                      Git revspec can be used." key)
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
  Term.(const run $ copts_t $ rootfs $ refspecs $ repo),
  Term.info "run" ~doc ~sdocs:copts_sect ~man

let default_cmd =
  let doc = "OCI client for Frama-C and Frama-C plugins" in
  let man = help_secs in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "bf_client" ~version:"0.1" ~sdocs:copts_sect ~doc ~man

let cmds = [list_rootfs_cmd; run_cmd]

let () =
  don't_wait_for begin
    match Term.eval_choice default_cmd cmds with
    | `Error _ -> exit 1
    | `Ok r -> r >>= fun () -> exit 0
    | `Help | `Version -> exit 0
  end

let () = never_returns (Scheduler.go ())
