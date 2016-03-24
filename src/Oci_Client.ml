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

module Git = struct
  type formatted_proc =
    Oci_Generic_Masters_Api.CompileGitRepoRunner.Formatted_proc.t

  let formatted_proc =
    Oci_Generic_Masters_Api.CompileGitRepoRunner.Formatted_proc.mk

  type env = [ `Extend of (string * string) list
             | `Replace of (string * string) list ]

  type args = [ `Proc of formatted_proc | `S of string ]

  let exec
      ?(args=[])
      ?(env=`Extend [])
      ?(proc_requested=1)
      ?(working_dir=Oci_Filename.current_dir)
      cmd =
    `Exec {Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd;
           args;proc_requested;working_dir;env}

  let run ?env ?proc_requested ?working_dir cmd args =
    exec ?env ?working_dir ?proc_requested
      cmd ~args:(List.map args ~f:(fun s -> `S s))

  let mk_proc s =
    `Proc (formatted_proc s)

  let make ?(j=1) ?(vars=[]) ?working_dir ?env targets =
    exec
      "make"
      ~args:((mk_proc "--jobs=%i") ::
             List.map vars ~f:(fun (var,v) -> `S (var^"="^v)) @
             List.map targets ~f:(fun s -> `S s))
      ?env
      ~proc_requested:j
      ?working_dir

  let git_clone ~url ?(dir=Oci_Filename.current_dir) commit =
    `GitClone
      {Oci_Generic_Masters_Api.CompileGitRepoRunner.url;directory=dir;commit}

  let git_copy_file ~url ~src ~dst commit =
    `GitCopyFile
      {Oci_Generic_Masters_Api.CompileGitRepoRunner.url;src;dst;commit}

  let copy_file ~checksum ~kind dst =
    `CopyFile
      {Oci_Generic_Masters_Api.CompileGitRepoRunner.kind;dst;checksum}

  type cmd = Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd
  type gitcopyfile = Oci_Generic_Masters_Api.CompileGitRepoRunner.gitcopyfile
  type gitclone = Oci_Generic_Masters_Api.CompileGitRepoRunner.gitclone
  type copyfile = Oci_Generic_Masters_Api.CompileGitRepoRunner.copyfile
  type repo = Oci_Generic_Masters_Api.CompileGitRepo.Query.repo

  let repo ?(save_artefact=true) ?(deps=[]) ?(cmds=[]) ?(tests=[]) () =
    {Oci_Generic_Masters_Api.CompileGitRepo.Query.deps;cmds;tests;save_artefact}

  type t = Oci_Generic_Masters_Api.CompileGitRepo.Query.t
  let repos ~name ~repos ~rootfs =
    {Oci_Generic_Masters_Api.CompileGitRepo.Query.name;rootfs;repos}

  let used_repos = Oci_Generic_Masters_Api.CompileGitRepo.Query.used_repos
  let filter_deps_for =
    Oci_Generic_Masters_Api.CompileGitRepo.Query.filter_deps_for
  let invariant = Oci_Generic_Masters_Api.CompileGitRepo.Query.invariant

  type compile_and_tests =
    [ `Artefact of Oci_Common.Artefact.t
    | `Cmd of
        Oci_Generic_Masters_Api.CompileGitRepoRunner.exec *
        (unit,
         Oci_Generic_Masters_Api.CompileGitRepoRunner.Result.exit_or_signal)
          Core.Std.Result.t * Oci_Common.Timed.t
    | `Dependency_error of Core.Std.String.Set.t ]

  let pp =
    Oci_Generic_Masters_Api.CompileGitRepo.Result.pp

  type connection = {
    socket: Async_extra.Import.Rpc_kernel.Connection.t;
    revspec_cache: (Oci_Generic_Masters_Api.GitCommitOfRevSpec.Query.t,
                    Oci_Common.Commit.t option Or_error.t Deferred.t)
        Hashtbl.Poly.t;
  }

  let socket_of_connection t = t.socket

  type exec = Oci_Generic_Masters_Api.CompileGitRepoRunner.exec = {
    cmd: string;
    args: [ `S of string | `Proc of formatted_proc ] list;
    env : [ `Replace of (string * string) list
          | `Extend of (string * string) list];
    proc_requested : int;
    working_dir: Oci_Filename.t (** Relative path *)
  }

  let compile_and_tests t repos =
    Rpc.Pipe_rpc.dispatch_exn
      (Oci_Data.log Oci_Generic_Masters_Api.CompileGitRepo.rpc)
      t.socket repos
    >>= fun (p,_) ->
    return p

  type xpra = [compile_and_tests | `XpraDir of Oci_Filename.t ]

  let pp_xpra = Oci_Generic_Masters_Api.XpraGitRepo.Result.pp

  let xpra t repos =
    Rpc.Pipe_rpc.dispatch_exn
      (Oci_Data.log Oci_Generic_Masters_Api.XpraGitRepo.rpc) t.socket repos
    >>= fun (p,_) ->
    return p

  let merge_base t ~url commit1 commit2 =
    Rpc.Rpc.dispatch_exn
      (Oci_Data.rpc Oci_Generic_Masters_Api.GitMergeBase.rpc)
      t.socket {Oci_Generic_Masters_Api.GitMergeBase.Query.url;commit1;commit2}
    >>= fun r ->
    return (Or_error.ok_exn r)

  let commit_of_revspec t ~url ~revspec =
    let key = {Oci_Generic_Masters_Api.GitCommitOfRevSpec.Query.url;revspec} in
    Hashtbl.Poly.find_or_add
      t.revspec_cache key
      ~default:(fun () ->
          Rpc.Rpc.dispatch_exn
            (Oci_Data.rpc Oci_Generic_Masters_Api.GitCommitOfRevSpec.rpc)
            t.socket key)
    >>= fun r ->
    return (Or_error.ok_exn r)

  let commit_of_revspec_exn t ~url ~revspec =
    commit_of_revspec t ~url ~revspec
    >>= function
    | None ->
      invalid_argf
        "revspec %s of %s correspond to no known ref"
        revspec url ()
    | Some commit ->
      return commit

  let commit_of_branch t ~url ~branch =
    Rpc.Rpc.dispatch_exn
      (Oci_Data.rpc Oci_Generic_Masters_Api.GitCommitOfBranch.rpc)
      t.socket {Oci_Generic_Masters_Api.GitCommitOfBranch.Query.url;branch}
    >>= fun r ->
    return (Or_error.ok_exn r)

  let last_commit_before t ~url ~branch ~time =
    Rpc.Rpc.dispatch_exn
      (Oci_Data.rpc Oci_Generic_Masters_Api.GitLastCommitBefore.rpc)
      t.socket
      {Oci_Generic_Masters_Api.GitLastCommitBefore.Query.url;branch;time}
    >>= fun r ->
    return (Or_error.ok_exn r)

  let time_of_commit t ~url ~commit =
    Rpc.Rpc.dispatch_exn
      (Oci_Data.rpc Oci_Generic_Masters_Api.GitTimeOfCommit.rpc)
      t.socket {Oci_Generic_Masters_Api.GitTimeOfCommit.Query.url;commit}
    >>= fun r ->
    return (Or_error.ok_exn r)


  let download_file t ~kind ~url ~checksum =
   Rpc.Rpc.dispatch_exn
      (Oci_Data.rpc Oci_Generic_Masters_Api.WgetDownloadFile.rpc)
      t.socket
      {Oci_Generic_Masters_Api.WgetDownloadFile.Query.url;kind;checksum}
    >>= fun r ->
    return (Or_error.ok_exn r)
end

module Gnuplot = struct

  let header = ["set title \"$BENCHS\"\n";
                "set xlabel \"time(s)\"\n";
                "set ylabel \"problems proved\"\n";
                "set key rmargin width -4 samplen 2\n";
                "plot \\\n"]
  let specification title =
    sprintf "\"-\" using 1:2 with steps title \"%s\", \\\n" title

  let print_header output writer datas =
    begin match output with
      | `Png(width,height,filename) ->
        Pipe.write_if_open writer
          (sprintf "set terminal pngcairo size %i, %i\n" width height)
        >>= fun () ->
        Pipe.write_if_open writer
          (sprintf "set output \"%s\"\n" filename)
      | `Svg(filename) ->
        Pipe.write_if_open writer "set terminal svg\n"
        >>= fun () ->
        Pipe.write_if_open writer
          (sprintf "set output \"%s\"\n" filename)
      | `Qt ->
        Pipe.write_if_open writer "set terminal qt persist\n"
    end
    >>= fun () ->
    Deferred.List.iter ~f:(Pipe.write_if_open writer) header
    >>= fun () ->
    Deferred.List.iter datas
      ~f:(fun (t,_) -> Pipe.write_if_open writer (specification t))
    >>= fun () ->
    Pipe.write_if_open writer "\n"

  let print_datas writer datas =
    Deferred.List.iter datas ~f:(fun (_,l) ->
        Deferred.List.iter l ~f:(fun (x,y) ->
            Pipe.write_if_open writer (sprintf "%f %i\n" x y)
          )
        >>= fun () ->
        Pipe.write_if_open writer "e\n"
      )

  let compute_datas_timeout timeout (l: (string * float list) list) =
    let rec aux acc current proved = function
      | [] -> List.rev ((current,proved)::acc)
      | a::_ when timeout < a ->
        List.rev ((current,proved)::acc)
      | a::l when Float.equal a current ->
        aux acc current (proved+1) l
      | a::l ->
        aux ((current,proved)::acc) a (proved+1) l
    in
    List.map l ~f:(fun (n,l) -> (n,aux [0.,0] 0. 0
                                   (List.sort ~cmp:Float.compare l)))

  let compute_datas_sort timeout (l: (string * float list) list) =
    let rec aux acc current proved = function
      | [] -> List.rev acc
      | a::_ when timeout < a -> List.rev acc
      | a::l ->
        aux ((current,proved)::acc) (current+.a) (proved+1) l
    in
    List.map l ~f:(fun (n,l) -> (n,aux [0.,0] 0. 0
                                   (List.sort ~cmp:Float.compare l)))

  let call_gnuplot filler =
    Process.create_exn ~prog:"gnuplot" ~args:["-"] ()
    >>= fun p ->
    let stdout = Reader.contents (Process.stdout p) in
    let stderr = Reader.contents (Process.stderr p) in
    filler (Writer.pipe (Process.stdin p))
    >>= fun () ->
    Writer.close (Process.stdin p)
    >>= fun () ->
    Process.wait p
    >>= fun result ->
    stdout
    >>= fun stdout ->
    stderr
    >>= fun stderr ->
    match result with
    | Ok () -> Deferred.unit
    | _ -> error "Gnuplot stopped unexpectedly with output:\n\
                  stderr:\n\
                  %s\n\
                  stdout:\n\
                  %s" stderr stdout;
      Shutdown.exit 1
end

(** module to sort *)
module Cmdline = struct

  let absolutize = Oci_Filename.make_absolute (Caml.Sys.getcwd ())

  let pp_kind fmt = function
    | Oci_Log.Chapter -> Format.fprintf fmt "P"
    | Oci_Log.Standard -> Format.fprintf fmt "S"
    | Oci_Log.Error -> Format.fprintf fmt "E"
    | Oci_Log.Command -> Format.fprintf fmt "C"

  let print_time fmt t =
    if Log.Global.level () = `Debug then Time.pp fmt t
    else Format.pp_print_string fmt
        (Time.format ~zone:Time.Zone.local t "%H:%M:%S")

  let exec_one test input ~init ~fold sexp_input output_printer conn =
    debug "Input %s\n%!" (Sexp.to_string_hum (sexp_input input));
    Rpc.Pipe_rpc.dispatch_exn (Oci_Data.log test) conn input
    >>= fun (p,_) ->
    let stdout = (Lazy.force Writer.stdout) in
    let fmt = Writer.to_formatter stdout in
    let open Textutils.Std in
    let complete = ref false in
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
          Format.fprintf fmt "[Result] %a@." output_printer r;
          return (fold acc (`Ok r))
        | {Oci_Log.data=Oci_Log.End (Error r)} ->
          Format.fprintf fmt
            "[Anomaly] please report: %s"
            (Sexp.to_string_hum (Error.sexp_of_t r));
          complete := true;
          return (fold acc (`Error r))
        | {Oci_Log.data=Oci_Log.End (Ok ())} ->
          complete := true;
          return acc
      )
    >>= fun res ->
    let res = if !complete then res else begin
        Format.fprintf fmt
          "[Anomaly] incomplete log, please report.";
        fold res `Incomplete
      end
    in
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

  let exec ?(init=`Ok)
      ?(fold=fun acc _ -> acc)
      test input sexp_input sexp_output conn =
    if Sys.getenv "OCIFORGET" = None
    then exec_one ~init ~fold test input sexp_input sexp_output conn.Git.socket
    else forget test input sexp_input sexp_output conn.Git.socket

  type default_revspec = {
    name: string;
    revspec: string;
  }

  type copts = { verb : Log.Level.t;  socket: string}


  let db_url_copy_file_md5sum = String.Table.create ()
  let add_copy_file_url kind checksum url =
    match kind with
    | `MD5 ->
      String.Table.add_multi db_url_copy_file_md5sum ~key:checksum ~data:url

  let mk_copy_file ~url ~checksum ~kind dst =
    List.iter url ~f:(add_copy_file_url kind checksum);
    Git.copy_file ~checksum ~kind dst

  let db_repos = ref String.Map.empty
  let url_to_default_revspec = String.Table.create ()

  let add_repo name repo =
    db_repos := String.Map.add !db_repos ~key:name ~data:repo

  let add_default_revspec_for_url ~revspec ~url ~name =
    String.Table.add_exn url_to_default_revspec
      ~key:url ~data:{name;revspec}

  type query = Oci_Generic_Masters_Api.CompileGitRepo.Query.t
  type revspecs = string option String.Map.t
  type ('x,'y) compare_n =
    Git.connection ->
    revspecs -> 'x -> 'y ->
    (revspecs * Git.repo * [`Exec of Git.exec ]) Deferred.t
  type exists_compare_n =
    | CompareN:
        ('x,'y) compare_n * (Sexp.t -> 'x) * ('x -> Sexp.t) * (Sexp.t -> 'y) *
        ('y -> Sexp.t) *
        (Unix.Exit_or_signal.t -> Oci_Common.Timed.t -> float option)
      -> exists_compare_n
  let db_compare_n = ref String.Map.empty

  let add_compare_n name compare_n sexp_x x_sexp sexp_y y_sexp analyse =
    db_compare_n :=
      String.Map.add !db_compare_n ~key:name
        ~data:(CompareN(compare_n,sexp_x,x_sexp,sexp_y,y_sexp,analyse))

  let default_create_query_hook ~connection:_
      ~query ~revspecs = Deferred.return (query, revspecs)

  let dumb_commit = Oci_Common.Commit.of_string_exn (String.make 40 '0')


  let create_query
      ?(info_level=`Info)
      create_query_hook rootfs revspecs repo repos connection =
    Rpc.Rpc.dispatch_exn (Oci_Data.rpc Oci_Rootfs_Api.find_rootfs)
      connection.Git.socket rootfs
    >>= fun rootfs ->
    Log.Global.printf ~level:info_level "Check the revspecs:";
    let query = Git.repos
        ~name:repo
        ~rootfs:(Or_error.ok_exn rootfs)
        ~repos
    in
    create_query_hook ~connection ~query ~revspecs
    >>= fun (query, revspecs) ->
    let used_repos = Git.used_repos query in
    let commits_cmdline = Buffer.create 100 in
    let get_commit url =
      let def = String.Table.find_exn url_to_default_revspec url in
      let revspec =
        Option.value ~default:def.revspec
          (String.Map.find_exn revspecs def.name)
      in
      Git.commit_of_revspec_exn ~url ~revspec connection
      >>= fun commit ->
      let msg =sprintf " --%s %s"
          def.name (Oci_Common.Commit.to_string commit)
      in
      Log.Global.printf ~level:info_level
        "%s: %s" def.name (Oci_Common.Commit.to_string commit);
      Buffer.add_string commits_cmdline msg;
      return commit
    in
    (* replace commit in repos *)
    Deferred.Map.map
      used_repos
      ~how:`Parallel
      ~f:(fun repo ->
          Deferred.List.map
            repo.Oci_Generic_Masters_Api.CompileGitRepo.Query.cmds
            ~f:(function
                | `GitClone (({commit}:Oci_Generic_Masters_Api.
                                         CompileGitRepoRunner.gitclone) as x)
                  when phys_equal commit dumb_commit ->
                  get_commit x.url
                  >>= fun commit ->
                  return (`GitClone {x with commit})
                | `GitCopyFile (({commit}:Oci_Generic_Masters_Api.
                                            CompileGitRepoRunner.gitcopyfile)
                                as x)
                  when phys_equal commit dumb_commit ->
                  get_commit x.url
                  >>= fun commit ->
                  return (`GitCopyFile {x with commit})
                | `CopyFile {Oci_Generic_Masters_Api.
                              CompileGitRepoRunner.checksum;kind} as x -> begin
                  match kind with
                  | `MD5 ->
                    match String.Table.find db_url_copy_file_md5sum
                            checksum with
                    | None -> Deferred.unit
                    | Some l ->
                      Deferred.List.find l
                        ~f:(fun url ->
                          Monitor.try_with_or_error ~here:[%here] (fun () ->
                                Git.download_file
                                  connection ~checksum ~kind ~url)
                          >>= function
                          | Ok () -> Deferred.return true
                          | Error e ->
                            error "Download error: %s" (Error.to_string_hum e);
                            Deferred.return false
                        )
                      >>= function
                      | None ->
                        error
                          "The file with checksum %s can't be downloaded"
                          checksum;
                        exit 1
                      | Some _ -> Deferred.unit
                  end
                  >>= fun () ->
                  Deferred.return x
                | x -> return x
              )
          >>= fun cmds ->
          return {repo with cmds}
        )
    >>= fun repos ->
    Log.Global.printf ~level:info_level
      "commits:%s" (Buffer.contents commits_cmdline);
    return { query with repos}

  let run cq_hook rootfs revspecs (repo:string) connection =
    create_query cq_hook rootfs revspecs repo !db_repos connection
    >>= fun query ->
    let fold acc = function
      | `Ok (`Cmd (_,Ok _,_)) -> acc
      | `Ok (`Cmd (_,_,_)) -> `Error
      | `Ok (`Dependency_error _) -> `Error
      | `Ok (`Artefact _) -> acc
      | `Error _ | `Incomplete -> `Error
    in
    exec Oci_Generic_Masters_Api.CompileGitRepo.rpc query ~fold
      Oci_Generic_Masters_Api.CompileGitRepo.Query.sexp_of_t
      Oci_Generic_Masters_Api.CompileGitRepo.Result.pp connection

  let xpra cq_hook rootfs revspecs repo socket =
    create_query cq_hook rootfs revspecs repo !db_repos socket
    >>= fun query ->
    let repos =
      String.Map.change query.repos repo ~f:(function
          | None -> assert false (* absurd: the main repo is used *)
          | Some data ->
            Some {data with tests=[]; cmds = List.filter ~f:(function
                | `GitClone _ -> true
                | `GitCopyFile _ -> true
                | `CopyFile _ -> true
                | `Exec _ -> false) data.cmds;
              };)
    in
    let query = { query with repos }
    in
    exec Oci_Generic_Masters_Api.XpraGitRepo.rpc query
      Oci_Generic_Masters_Api.CompileGitRepo.Query.sexp_of_t
      Oci_Generic_Masters_Api.XpraGitRepo.Result.pp socket

  let compare_n cq_hook rootfs revspecs
      (x_input:Oci_Filename.t) (y_input:Oci_Filename.t) bench
      outputs summation connection =
    match String.Map.find_exn !db_compare_n bench with
    | CompareN(for_one,sexp_x,x_sexp,sexp_y,y_sexp,analyse) ->
      Reader.load_sexps_exn x_input sexp_x
      >>= fun lx ->
      Reader.load_sexps_exn y_input sexp_y
      >>= fun ly ->
      let exec_one x y =
        for_one connection revspecs x y
        >>= function (revspecs, git_repo, `Exec exec) ->
        let repo_compare_n = "Oci_Client.compare_n" in
        let repos = String.Map.add !db_repos ~key:repo_compare_n
            ~data:git_repo in
        create_query
          ~info_level:`Debug
          cq_hook rootfs revspecs repo_compare_n
          repos connection
        >>= fun query ->
        Rpc.Pipe_rpc.dispatch_exn
          (Oci_Data.log Oci_Generic_Masters_Api.CompileGitRepo.rpc)
          connection.Git.socket query
        >>= fun (p,_) ->
        let complete = ref false in
        Pipe.fold p ~init:None ~f:(fun acc -> function
            | {Oci_Log.data=Oci_Log.Std (_,line);_} ->
              debug "[Std] %s" line;
              return acc
            | {Oci_Log.data=Oci_Log.Extra r} ->
              begin match r with
                | `Cmd(exec',result,time) as r when
                    Oci_Generic_Masters_Api.CompileGitRepoRunner.
                      compare_exec exec exec' = 0
                  ->
                  debug
                    "[Result] %s@\n%s@."
                    (Sexp.to_string_hum (
                        Oci_Generic_Masters_Api.CompileGitRepo.
                          Query.sexp_of_t query))
                    (Oci_pp.string_of
                       Oci_Generic_Masters_Api.CompileGitRepo.Result.pp r);
                  return (analyse result time)
                | r ->
                  debug
                    "[Prepare] %s@."
                    (Oci_pp.string_of
                       Oci_Generic_Masters_Api.CompileGitRepo.Result.pp r);
                  return acc
              end
            | {Oci_Log.data=Oci_Log.End (Error e)} ->
              error
                "[Anomaly] for %s please report: %s"
                (Sexp.to_string_hum (
                    Oci_Generic_Masters_Api.CompileGitRepo.
                      Query.sexp_of_t query))
                (Sexp.to_string_hum (Error.sexp_of_t e));
              complete := true;
              return None
            | {Oci_Log.data=Oci_Log.End (Ok ())} ->
              complete := true;
              return acc
          )
        >>= fun res ->
        return (if !complete then res else
                  (error
                     "[Anomaly] incomplete log for %s"
                     (Sexp.to_string_hum (
                         Oci_Generic_Masters_Api.CompileGitRepo.
                           Query.sexp_of_t query));
                   None))
      in
      Deferred.List.map
        ~how:`Parallel
        lx ~f:(fun x ->
            Deferred.List.map
              ~how:`Parallel ly
              ~f:(fun y -> exec_one x y
                   >>= function
                   | None ->
                     error "Bad result for %s %s"
                       (Sexp.to_string (x_sexp x)) (Sexp.to_string (y_sexp y));
                     return None
                   | x -> return x
                 )
            >>= fun res ->
            return (x,res)
          )
      >>= fun results ->
      let results = List.map results ~f:(fun (x,l) -> (x,List.filter_opt l)) in
      let datas = lazy begin
        let results =
          List.map ~f:(fun (x,l) -> Sexp.to_string (x_sexp x),l)
            results
        in
        match summation with
        | `Timeout -> Gnuplot.compute_datas_timeout  2. results
        | `Sort -> Gnuplot.compute_datas_sort  2. results
      end in
      let gnuplot call mode =
        let datas = Lazy.force datas in
        call (fun writer ->
            Gnuplot.print_header mode writer datas
            >>= fun () ->
            Gnuplot.print_datas writer datas
          )
      in
      Deferred.List.iter outputs ~f:(function
          | `Sexp filename ->
            let sexp =
              List.sexp_of_t
                (sexp_of_pair x_sexp (List.sexp_of_t Float.sexp_of_t)) in
            Writer.save_sexp ~hum:true filename (sexp results)
          | `Gnuplot filename ->
            let call filler =
              Writer.open_file filename
              >>= fun writer ->
              filler (Writer.pipe writer)
              >>= fun () ->
              Writer.close writer
            in
            gnuplot call (`Svg("foobar.svg"))
          | `Png filename ->
            gnuplot Gnuplot.call_gnuplot (`Png(640,480,filename))
          | `Svg filename ->
            gnuplot Gnuplot.call_gnuplot (`Svg(filename))
          | `Qt ->
            gnuplot Gnuplot.call_gnuplot `Qt
        )
      >>= fun () ->
      Deferred.return `Ok

  let list_rootfs rootfs socket =
    let open Oci_Rootfs_Api in
    Deferred.List.fold rootfs
      ~init:`Ok
      ~f:(fun acc rootfs ->
          exec find_rootfs rootfs
            ~init:acc
            Rootfs_Id.sexp_of_t
            (Oci_pp.to_sexp Rootfs.sexp_of_t) socket
        )

  let add_packages rootfs packages =
    let open Oci_Rootfs_Api in
    exec add_packages
      { id = rootfs;
        packages = packages;
      }
      sexp_of_add_packages_query
      (Oci_pp.to_sexp Rootfs.sexp_of_t)

  let create_rootfs rootfs_tar meta_tar distribution release arch comment =
    let open Oci_Rootfs_Api in
    exec create_rootfs
      { meta_tar = Option.map ~f:absolutize meta_tar;
        rootfs_tar = absolutize rootfs_tar;
        rootfs_info = { distribution;
                        release;
                        arch;
                        packages = [];
                        comment;
                      }
      }
      sexp_of_create_rootfs_query
      (Oci_pp.to_sexp Rootfs.sexp_of_t)

  let connect ccopt cmd =
    Tcp.connect (Tcp.to_file ccopt.socket)
    >>= fun (_,reader,writer) ->
    Rpc.Connection.create
      ~connection_state:(fun _ -> ())
      ~description:(Info.of_string "Client <-> Master")
      reader writer
    >>= fun socket ->
    let socket = Result.ok_exn socket in
    cmd {Git.socket;revspec_cache=Hashtbl.Poly.create ()}
    >>= fun res ->
    Rpc.Connection.close socket
    >>= fun () ->
    return res

  module Download_Rootfs = struct
    (** {2 GPG} *)

    let download_keyid = "0xBAEFF88C22F6E216"
    let download_keyserver = "hkp://pool.sks-keyservers.net"


    let gpg_setup ~dir =
      Async_shell.test "which" ["gpg"]
      >>= function
      | false ->
        error "The program gpg is not present: can't validate download";
        return `GPGNotAvailable
      | true ->
        let gpg_dir = Filename.concat dir "gpg" in
        Unix.mkdir ~p:()  ~perm:0o700 gpg_dir
        >>= fun () ->
        Async_shell.test
          "gpg" ["--keyserver";download_keyserver;"--recv-keys";download_keyid]
          ~env:(`Extend ["GNUPGHOME",gpg_dir])
        >>= function
        | false ->
          error "Can't download gpg key data: can't validate download";
          return `GPGNotAvailable
        | true ->
          return (`GPGAvailable gpg_dir)

    let gpg_check file = function
      | `GPGNotAvailable -> return ()
      | `GPGAvailable gpg_dir ->
        Async_shell.test
          "gpg" ["--verify";(file^".asc")]
          ~env:(`Extend ["GNUPGHOME",gpg_dir])
        >>= function
        | false ->
          error "The downloaded file are not authenticated";
          exit 1
        | true -> return ()

    (** {2 Download image} *)
    (** use lxc download template facilities *)

    let download_compat_level=2
    let download_server = "images.linuxcontainers.org"

    let download fmt =
      Printf.ksprintf (fun src ->
          Printf.ksprintf (fun dst ->
              Async_shell.test
                ~true_v:[0]
                ~false_v:[1;4;8;2;3;5;6;7]
                "wget"
                ["-T";"30";
                 "-q";
                 sprintf "https://%s/%s" download_server src;
                 "-O";dst]
            )
        ) fmt

    let download_index ~dir ~gpg =
      let index = Filename.concat dir "index" in
      let url_index = "meta/1.0/index-user" in
      info "Download the index.";
      download
        "%s.%i" url_index download_compat_level
        "%s" index
      >>= fun b1 ->
      download
        "%s.%i.asc" url_index download_compat_level
        "%s.asc" index
      >>= fun b2 -> begin
        if b1 && b2 then
          return ()
        else begin
          download
            "%s" url_index
            "%s" index
          >>= fun b1 ->
          download
            "%s.asc" url_index
            "%s.asc" index
          >>= fun b2 ->
          if b1 && b2 then
            return ()
          else begin
            error "Can't download rootfs index";
            exit 1
          end
        end
      end
      >>= fun () ->
      gpg_check index gpg;
      >>= fun () ->
      info "Index downloading done.\n%!";
      return index

    type rootfs_spec =
      {distrib: string; release: string;
       arch: string; comment: string}
    [@@deriving bin_io, sexp]

    (** return download build and directory url *)
    let parse_index index =
      Reader.file_lines index
      >>= fun lines ->
      let fold acc l =
        match String.split ~on:';' l with
        | [distrib;release;arch;comment;build_id;url] ->
          ({distrib;release;arch;comment},(build_id,url))::acc
        | _ -> acc in
      let l = List.fold ~init:[] ~f:fold lines in
      return (List.rev l)

    let download_rootfs_meta ~dir ~gpg (_build_id,url) =
      (* let build_id_file = Filename.concat dir "build_id" in *)
      let rootfs_tar = Filename.concat dir "rootfs.tar.xz" in
      let meta_tar = Filename.concat dir "meta.tar.xz" in
      (* if not (Sys.file_exists_exn build_id_file) *)
      (* || read_in_file "%s" build_id_file <> build_id then begin *)
      (*   if Sys.file_exists build_id_file then Unix.unlink build_id_file; *)
      info "Downloading rootfs.\n%!";
      Deferred.all [
        download "%s/rootfs.tar.xz" url "%s/rootfs.tar.xz" dir;
        download "%s/rootfs.tar.xz.asc" url "%s/rootfs.tar.xz.asc" dir;
        download "%s/meta.tar.xz" url "%s/meta.tar.xz" dir;
        download "%s/meta.tar.xz.asc" url "%s/meta.tar.xz.asc" dir
      ]
      >>= fun l ->
      if List.for_all ~f:(fun x -> x) l
      then
        Deferred.all_ignore [
          gpg_check rootfs_tar gpg;
          gpg_check meta_tar gpg
        ]
        >>= fun () ->
        (* write_in_file "%s" build_id_file "%s" build_id *)
        return (rootfs_tar, meta_tar)
      else begin error "Can't download rootfs"; exit 1 end

    let list_download_rootfs () =
      let testdir = ".oci_tmp" in
      Unix.mkdir ~p:() testdir
      >>= fun () ->
      gpg_setup ~dir:testdir
      >>= fun gpg ->
      download_index ~dir:testdir ~gpg
      >>= fun index ->
      parse_index index
      >>= fun archives ->
      let stdout = (Lazy.force Writer.stdout) in
      let fmt = Writer.to_formatter stdout in
      let print = Format.fprintf fmt "%s, %s, %s@." in
      print "distribution" "release" "arch";
      List.iter archives
        ~f:(fun (key,_) -> print key.distrib key.release key.arch);
      Deferred.return `Ok

    let download_rootfs distrib release arch comment socket =
      let testdir = ".oci_tmp" in
      Unix.mkdir ~p:() testdir
      >>= fun () ->
      gpg_setup ~dir:testdir
      >>= fun gpg ->
      download_index ~dir:testdir ~gpg
      >>= fun index ->
      parse_index index
      >>= fun archives ->
      let d = {distrib;release;arch;comment} in
      match List.find_map
              ~f:(fun (x,b) -> if x=d then Some b else None) archives
      with
      | None ->
        error "Specified rootfs not found in index";
        exit 1
      | Some build_id_url ->
        download_rootfs_meta ~dir:testdir ~gpg build_id_url
        >>= fun (rootfs_tar, meta_tar) ->
        create_rootfs rootfs_tar (Some meta_tar)
          distrib release arch comment socket
  end

  open Cmdliner

  (** Commandline handling *)
  (* Help sections common to all commands *)

  let copts_sect = "COMMON OPTIONS"
  let help_secs = [
    `S copts_sect;
    `P "These options are common to all commands.";
    `S "MORE HELP";
    `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
    `Noblank;
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
    Term.(const list_rootfs $ rootfs),
    Term.info "list-rootfs" ~sdocs:copts_sect ~doc ~man

  let create_rootfs_cmd =
    let rootfs_tar =
      Arg.(required & opt (some file) None & info ["rootfs"]
             ~docv:"TAR"
             ~doc:"Give the rootfs archive to use")
    in
    let meta_tar =
      Arg.(value & opt (some file) None & info ["meta"]
             ~docv:"TAR"
             ~doc:"Give the meta archive to use")
    in
    let distribution =
      Arg.(value & opt string "unknown" & info ["distribution"]
             ~docv:"name"
             ~doc:"Indicate the name of the distribution in the given rootfs")
    in
    let release =
      Arg.(value & opt string "" & info ["release"]
             ~docv:"name"
             ~doc:"Indicate the release name of the distribution \
                   in the given rootfs")
    in
    let arch =
      Arg.(value & opt string "" & info ["arch"]
             ~docv:"name"
             ~doc:"Indicate the target architecture in the given rootfs")
    in
    let comment =
      Arg.(value & opt string "" & info ["comment"]
             ~docv:"text"
             ~doc:"Specify some comment about in the given rootfs")
    in
    let doc = "Create a new rootfs from an archive" in
    let man = [
      `S "DESCRIPTION";
      `P "Create a rootfs from an archive downloaded from \
          lxc download mirrors."]
      @ help_secs
    in
    Term.(const create_rootfs $ rootfs_tar $ meta_tar $
          distribution $ release $ arch $ comment),
    Term.info "create-rootfs" ~sdocs:copts_sect ~doc ~man

  let download_rootfs_cmd =
    let distribution =
      Arg.(value & opt string "debian" & info ["distribution"]
             ~docv:"name"
             ~doc:"Indicate the name of the distribution in the given rootfs")
    in
    let release =
      Arg.(value & opt string "jessie" & info ["release"]
             ~docv:"name"
             ~doc:"Indicate the release name of the distribution \
                   in the given rootfs")
    in
    let arch =
      Arg.(value & opt string "amd64" & info ["arch"]
             ~docv:"name"
             ~doc:"Indicate the target architecture in the given rootfs")
    in
    let comment =
      Arg.(value & opt string "default" & info ["comment"]
             ~docv:"text"
             ~doc:"Specify some comment about in the given rootfs")
    in
    let doc = "Create a new rootfs from an archive" in
    let man = [
      `S "DESCRIPTION";
      `P "Create a rootfs from an archive downloaded from \
          lxc download mirrors."]
      @ help_secs
    in
    Term.(const Download_Rootfs.download_rootfs $
          distribution $ release $ arch $ comment),
    Term.info "download-rootfs" ~sdocs:copts_sect ~doc ~man

  let list_download_rootfs_cmd =
    let doc = "list the rootfs available from lxc download mirror" in
    let man = [
      `S "DESCRIPTION";
      `P "Return the list the rootfs available from lxc download mirror"]
      @ help_secs
    in
    Term.(const Download_Rootfs.list_download_rootfs $ (const ())),
    Term.info "list-download-rootfs" ~sdocs:copts_sect ~doc ~man

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
    Term.(const add_packages $ rootfs $ packages),
    Term.info "add-package" ~sdocs:copts_sect ~doc ~man

  let cmdliner_revspecs init =
    String.Table.fold url_to_default_revspec
      ~init:Term.(const init)
      ~f:(fun ~key:_ ~data acc ->
          let spec = (fun s -> `Ok (Some s)),
                     (fun fmt -> function
                        | None -> Format.pp_print_string fmt data.revspec
                        | Some s -> Format.pp_print_string fmt s)
          in
          Term.(const (fun revspec acc ->
              String.Map.add ~key:data.name ~data:revspec acc) $
                Arg.(value & opt spec None & info [data.name]
                       ~docv:"REVSPEC"
                       ~doc:(sprintf "indicate which revspec of %s to use."
                               data.name)
                    ) $
                acc)
        )

  let rootfs =
    Arg.(required & opt (some rootfs_converter) None & info ["rootfs"]
           ~docv:"ID"
           ~doc:"Specify on which rootfs to run")

  let run_xpra_cmd create_query_hook =
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
      `P "Run the integration of the given repository with the given rootfs \
          using the specified commits."] @ help_secs
    in
    [Term.(const run $ create_query_hook $ rootfs $
           (cmdliner_revspecs String.Map.empty) $ repo),
     Term.info "run" ~doc ~sdocs:copts_sect ~man;
     Term.(const xpra $ create_query_hook $ rootfs $
           (cmdliner_revspecs String.Map.empty) $ repo),
     Term.info "xpra" ~doc ~sdocs:copts_sect ~man]

  let compare_n_cmd create_query_hook =
    let bench =
      let benchs = String.Map.keys !db_compare_n in
      let benchs_enum = List.map ~f:(fun x -> (x,x)) benchs in
      Arg.(required & pos 0 (some (enum benchs_enum)) None & info []
             ~docv:"REPO_NAME"
             ~doc:("Run the repository $(docv). \
                    Possible values: " ^
                   (String.concat ~sep:", " benchs) ^ "."))
    in
    let x_input =
      Arg.(required & opt (some file) None & info ["x-input"]
             ~docv:"X"
             ~doc:"Give the list of x variables")
    in
    let y_input =
      Arg.(required & opt (some file) None & info ["y-input"]
             ~docv:"Y"
             ~doc:"Give the list of y variables")
    in
    let outputs =
      let output kind =
        Arg.(value & opt (some string) None & info [sprintf "output-%s" kind]
               ~docv:"FILE"
               ~doc:(sprintf
                       "Specify the file to output the computed datas \
                        in %s format" kind))
      in
      let show_qt =
        let doc = "Specify to show the graphic in a window" in
        Arg.(value & vflag None [Some "", info ["show-qt"] ~doc])
      in
      Term.(List.fold
              ~init:(const [])
              ~f:(fun acc (term,conv) ->
                  const (fun acc -> function
                      | None -> acc
                      | Some v -> (conv v)::acc) $ acc $ term
                )
              [ output "sexp", (fun s -> `Sexp s);
                output "png", (fun s -> `Png s);
                output "svg", (fun s -> `Svg s);
                output "gpl", (fun s -> `Gnuplot s);
                show_qt, (fun _ -> `Qt)])
    in
    let summation =
      Arg.(value & vflag `Sort
             [`Timeout, info ["summation-by-timeout"]
                ~doc:"For a given time compute the number of run that finish \
                      before that time. It is simpler than --summation-by-sort \
                      but the end of the curve depends less of the time taken \
                      by fast runs.";
              `Sort, info ["summation-by-sort"]
                ~doc:"For a given time compute the maximal number of \
                      run that could be run sequentially in the given time. \
                      It is the default.";
             ])
    in
    let doc = "run a specific benchmark" in
    let man = [
      `S "DESCRIPTION";
      `P "Run the benchmark for comparing the x with the y\
          (TODO: find a name for x and y)."] @ help_secs
    in
    Term.(const compare_n $
          create_query_hook $ rootfs $
          (cmdliner_revspecs String.Map.empty) $ x_input $ y_input $ bench
          $ outputs $ summation),
    Term.info "compare_n" ~doc ~sdocs:copts_sect ~man

  let default_cmd ?version ?doc name =
    let man = help_secs in
    Term.(ret (const (`Help (`Pager, None)))),
    Term.info name ?version ?doc ~man

  let def_cmds_with_connection create_query_hook =
    [list_rootfs_cmd; create_rootfs_cmd;
     add_package_cmd;
     download_rootfs_cmd]@
    (if String.Map.is_empty !db_compare_n
     then [] else [compare_n_cmd create_query_hook])@
    (run_xpra_cmd create_query_hook)

  let def_cmds_without_connection =
    [list_download_rootfs_cmd]

  type create_query_hook =
    (connection:Git.connection ->
     query:query -> revspecs:revspecs ->
     (query * revspecs) Deferred.t) Cmdliner.Term.t

  type cmds_without_connection =
    [ `Error | `Ok ] Async.Std.Deferred.t Cmdliner.Term.t *
    Cmdliner.Term.info

  type cmds_with_connection =
    (Git.connection ->
     [ `Error | `Ok ] Async.Std.Deferred.t) Cmdliner.Term.t *
    Cmdliner.Term.info

  let default_cmdline
      ?(create_query_hook=Term.const default_create_query_hook)
      ?(cmds_without_connections=[])
      ?(cmds_with_connections=[])
      ?doc ?version name =
    let cmds =
      List.map
        ~f:(fun (f,i) ->
            Term.(const (fun ccopt f -> connect ccopt f) $ copts_t $ f),i)
        (def_cmds_with_connection create_query_hook@cmds_with_connections) @
      (def_cmds_without_connection@cmds_without_connections)
    in
    match Term.eval_choice (default_cmd ?version ?doc name)
            cmds with
    | `Error _ -> exit 1
    | `Ok r -> begin
        r >>= function
        | `Ok -> Shutdown.exit 0
        | `Error -> Shutdown.exit 1
      end
    | `Help | `Version -> exit 0

  let git_clone ?dir url =
    if not (String.Table.mem url_to_default_revspec url)
    then invalid_argf
        "The url have not been registered with \
         add_default_revspec_for_url: %s" url ();
    Git.git_clone ?dir ~url dumb_commit

  let git_copy_file ~src ~dst url =
    if not (String.Table.mem url_to_default_revspec url)
    then invalid_argf
        "The url have not been registered with \
         add_default_revspec_for_url: %s" url ();
    Git.git_copy_file ~src ~dst ~url dumb_commit

  type repo = {name:string;url:string}

  let mk_repo ?(revspec="master") ~url ~deps ~cmds ?(tests=[]) name =
    let id = {name;url} in
    add_default_revspec_for_url
      ~url ~revspec ~name;
    add_repo
      name
      (Git.repo
         ~deps:(List.map ~f:(fun x -> x.name) deps)
         ~cmds:((git_clone url)::cmds)
         ~tests
         ());
    id

  let mk_compare_n
      ~deps
      ~cmds
      ~x_of_sexp ~sexp_of_x ~y_of_sexp ~sexp_of_y ~analyse
      name =
    add_compare_n
      name
      (fun conn revspecs x y ->
         cmds conn revspecs x y
         >>= fun (revspecs,cmds,test) ->
         return
           (revspecs,
            Git.repo
              ~save_artefact:false
              ~deps:(List.map ~f:(fun x -> x.name) deps)
              ~cmds
              ~tests:[(test:> Oci_Generic_Masters_Api.CompileGitRepoRunner.
                                cmd)]
              (),
            test)
      )
      x_of_sexp sexp_of_x y_of_sexp sexp_of_y analyse

  module Predefined = struct
    open Git
    let ocaml = mk_repo
        "ocaml"
        ~url:"https://github.com/ocaml/ocaml.git"
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

    let ocamlfind = mk_repo
        "ocamlfind"
        ~url:"https://gitlab.camlcity.org/gerd/lib-findlib.git"
        ~deps:[ocaml]
        ~cmds:[
          run "./configure" [];
          make ["all"];
          make ["opt"];
          make ["install"];
        ]

    let ocamlbuild = mk_repo
        "ocamlbuild"
        ~url:"https://github.com/ocaml/ocamlbuild.git"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          (* don't compile and install ocamlbuild if it is already installed
              (before ocaml 4.03) *)
          run "sh" ["-c"; "which ocamlbuild || make"];
          run "sh" ["-c"; "which ocamlbuild || make install"];
        ]

    let zarith = mk_repo
        "ZArith"
        ~url:"https://github.com/bobot/zarith.git"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          run "./configure" [];
          make [];
          make ["install"];
        ]

    let xmllight = mk_repo
        "xml-light"
        ~url:"https://github.com/ncannasse/xml-light.git"
        ~revspec:"2.4"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          make ["install_ocamlfind"]
        ]

    let camlp4 = mk_repo
        "camlp4"
        ~url:"https://github.com/ocaml/camlp4.git"
        ~revspec:"4.02+6"
        ~deps:[ocaml;ocamlfind;ocamlbuild]
        ~cmds:[
          (* don't compile and install camlp4 if it is already installed
              (before ocaml 4.02) *)
          run "sh" ["-c"; "which camlp4 || ./configure"];
          run "sh" ["-c"; "which camlp4 || make all"];
          run "sh" ["-c"; "which camlp4 || make install install-META"];
        ]

    let lablgtk = mk_repo
        "lablgtk"
        ~url:"https://forge.ocamlcore.org/anonscm/git/lablgtk/lablgtk.git"
        ~revspec:"28290b0ee79817510bbc908bc733e80258aea7c1"
        ~deps:[ocaml;ocamlfind;camlp4]
        ~cmds:[
          run "./configure" [];
          make ["world"];
          make ~env:(`Extend ["OCAMLFIND_LDCONF","ignore"]) ["install"];
        ]

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

    let cppo = mk_repo
        "cppo"
        ~url:"https://github.com/mjambon/cppo.git"
        ~deps:[ocaml;ocamlfind;ocamlbuild]
        ~cmds:[
          make [];
          make ["install"];
        ]

    let camomile =
      let working_dir = "Camomile" in
      mk_repo
        "camomile"
        ~url:"https://github.com/yoriyuki/Camomile.git"
        (* latest stable release, 0.8.5 *)
        ~revspec:"07415d3049eaad11914523468904ee117db149e3"
        ~deps:[ocaml;ocamlfind;camlp4;cppo]
        ~cmds:[
          run ~working_dir "autoconf" [];
          run ~working_dir "./configure" [];
          make ~working_dir [];
          make ~working_dir ["install"];
        ]

    let ounit =
      let r = Git.repo
        ~deps:[ocaml.name;ocamlfind.name;ocamlbuild.name]
        ~cmds:[
          mk_copy_file
            ~url:["http://forge.ocamlcore.org/frs/download.php/1258/\
                   ounit-2.0.0.tar.gz"]
            ~kind:`MD5
            ~checksum:"2e0a24648c55005978d4923eb4925b28"
            "ounit.tar.gz" ;
          run "tar" ["xf";"ounit.tar.gz";"--strip-component";"1"];
          make ["build"];
          make ["install"];
        ]
        ()
      in
      add_repo "ounit" r;
      {name="ounit";url=""}

    let cryptokit = mk_repo
        "cryptokit"
        ~url:"git@git.frama-c.com:bobot/CryptoKit.git"
        ~revspec:"87a8f3597c11381c2f7361b96f952913a4d66f3c"
        ~deps:[ocaml;ocamlfind;ocamlbuild]
        ~cmds:[
          make [];
          make ["install"];
        ]

  end

end

let oci_version = Oci_Version.version
