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
      ?timelimit
      ?memlimit
      cmd =
    `Exec {Oci_Generic_Masters_Api.CompileGitRepoRunner.cmd;
           args;proc_requested;working_dir;env;timelimit;memlimit}

  let run ?env ?proc_requested ?working_dir ?timelimit ?memlimit cmd args =
    exec ?env ?working_dir ?proc_requested ?timelimit ?memlimit
      cmd ~args:(List.map args ~f:(fun s -> `S s))

  let mk_proc s =
    `Proc (formatted_proc s)

  let make ?(j=1) ?(vars=[]) ?working_dir ?env ?timelimit ?memlimit targets =
    exec
      "make"
      ~args:((mk_proc "--jobs=%i") ::
             List.map vars ~f:(fun (var,v) -> `S (var^"="^v)) @
             List.map targets ~f:(fun s -> `S s))
      ?env
      ~proc_requested:j
      ?working_dir ?timelimit ?memlimit 

  let git_clone ~url ?(dir=Oci_Filename.current_dir) commit =
    `GitClone
      {Oci_Generic_Masters_Api.CompileGitRepoRunner.url;directory=dir;commit}

  let git_copy_file ~url ~src ~dst commit =
    `GitCopyFile
      {Oci_Generic_Masters_Api.CompileGitRepoRunner.url;src;dst;commit}

  let copy_file ~checksum ~kind dst =
    `LinkFile
      {Oci_Generic_Masters_Api.CompileGitRepoRunner.kind;dst;checksum}

  let copy_file_from_zip ~checksum ~kind ~src dst =
    let dst_archive = "archive.zip" in
    let unzip_dir = "oci.archive.tmp" in
    [copy_file ~checksum ~kind dst_archive;
     run "unzip" [dst_archive;src;"-d";unzip_dir];
     run "mv" [Oci_Filename.concat unzip_dir src;dst];
    ]

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
    (* Oci_Generic_Masters_Api.CompileGitRepo.Result.t = *)
    [
      | `CmdStart of Oci_Generic_Masters_Api.CompileGitRepoRunner.exec
      | `Cmd of Oci_Generic_Masters_Api.CompileGitRepoRunner.exec
                * Unix.Exit_or_signal.t * Oci_Common.Timed.t
      | `Artefact of Oci_Common.Artefact.t
      | `Dependency_error of String.Set.t
    ]

  let pp =
    Oci_Generic_Masters_Api.CompileGitRepo.Result.pp

  type connection = {
    socket: Async_extra.Import.Rpc_kernel.Connection.t;
    revspec_cache: (Oci_Generic_Masters_Api.GitCommitOfRevSpec.Query.t,
                    Oci_Common.Commit.t option Or_error.t Deferred.t)
        Hashtbl.Poly.t;
    download_cache: (Oci_Generic_Masters_Api.WgetDownloadFile.Query.t,
                     unit Or_error.t Deferred.t)
        Hashtbl.Poly.t;
  }

  let socket_of_connection t = t.socket

  type exec = Oci_Generic_Masters_Api.CompileGitRepoRunner.exec = {
    cmd: string;
    args: [ `S of string | `Proc of formatted_proc ] list;
    env : [ `Replace of (string * string) list
          | `Extend of (string * string) list];
    proc_requested : int;
    working_dir: Oci_Filename.t (** Relative path *);
    timelimit: Time.Span.t option;
    memlimit: Byte_units.t option;
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
    let key =
      {Oci_Generic_Masters_Api.WgetDownloadFile.Query.url;kind;checksum}
    in
    Hashtbl.Poly.find_or_add
      t.download_cache key
      ~default:(fun () ->
          Rpc.Rpc.dispatch_exn
            (Oci_Data.rpc Oci_Generic_Masters_Api.WgetDownloadFile.rpc)
            t.socket key)
    >>= fun r ->
    return (Or_error.ok_exn r)
end

module Gnuplot = struct

  let print_output output writer =
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

  let print_datas writer data =
    Deferred.List.iter data ~f:(fun (x,y) ->
        Pipe.write_if_open writer (sprintf "%f %f\n" x y)
      )
    >>= fun () ->
    Pipe.write_if_open writer "e\n"

  let steps_filler datas mode writer =
    print_output mode writer
    >>= fun () ->
    Pipe.write_if_open writer
      "set title \"Comparison\"\n\
       set xlabel \"time(s)\"\n\
       set ylabel \"problems proved\"\n\
       set key rmargin width -4 samplen 2\n\
       plot \\\n"
    >>= fun () ->
    Deferred.List.iter datas
      ~f:(fun (t,_) ->
          Pipe.write_if_open writer
            (sprintf "\"-\" using 1:2 with steps title \"%s\" noenhanced,\
                      \\\n" t))
    >>= fun () ->
    Pipe.write_if_open writer "\n"
    >>= fun () ->
    Deferred.List.iter datas ~f:(fun (_,data) ->
        print_datas writer data
      )

  let compute_datas_timeout timeout (l: (string * float list) list) =
    let rec aux acc current proved = function
      | [] -> List.rev ((current,float proved)::acc)
      | a::_ when timeout < a ->
        List.rev ((current,float proved)::acc)
      | a::l when Float.equal a current ->
        aux acc current (proved+1) l
      | a::l ->
        aux ((current,float proved)::acc) a (proved+1) l
    in
    let r =
      List.map l ~f:(fun (n,l) -> (n,aux [] 0. 0
                                     (List.sort ~cmp:Float.compare l)))
    in
    steps_filler r

  let compute_datas_sort timeout (l: (string * float list) list) =
    let rec aux acc current proved = function
      | [] -> List.rev acc
      | a::_ when timeout < a -> List.rev acc
      | a::l ->
        aux ((current,float proved)::acc) (current+.a) (proved+1) l
    in
    let r =
      List.map l ~f:(fun (n,l) -> (n,aux [0.,0.] 0. 0
                                     (List.sort ~cmp:Float.compare l)))
    in
    steps_filler r

  let error_measure = 0.04

  let compute_datas_two timeout (a,la) (b,lb) =
    let r = List.map2_exn la lb ~f:(fun x y ->
        match x,y with
        | None, _ | _, None -> None
        | Some x, Some y -> Some (max error_measure (min timeout x),
                                  max error_measure (min timeout y))
      ) in
    let r = List.filter_opt r in
    (fun mode writer ->
       print_output mode writer
       >>= fun () ->
       Pipe.write_if_open writer
         (sprintf "set title \"Comparison\"\n\
	           set logscale xy 10\n\
	           set xrange [%f:%f]\n\
                   set yrange [%f:%f]\n\
                   set xlabel \"%s\" noenhanced\n\
                   set ylabel \"%s\" noenhanced\n\
                   set key rmargin width -4 samplen 2\n"
            error_measure timeout error_measure timeout
            a b)
       >>= fun () ->
       Pipe.write_if_open writer
         (sprintf "plot \"-\" using 1:2 title \"bench\",\\\n\
          x+%f with lines linecolor rgbcolor \"green\", x-%f with lines \\\n\
          linecolor rgbcolor \"green\"\n" error_measure error_measure)
       >>= fun () ->
       print_datas writer r
    )

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


  module WP = struct
    type ('a,'b) param = {
      default: 'a;
      id: 'a Type_equal.Id.t;
      of_sexp: (Sexp.t -> 'a) option;
      to_option_hum: 'a -> string;
      cmdliner: 'a Cmdliner.Term.t;
      resolve: (Git.connection -> 'a -> 'b Deferred.t) with_param;
      unresolve: 'b -> 'a;
    }
    and
      _ with_param =
      | WP_Const: 'a -> 'a with_param
      | WP_Param: (_,'b) param -> 'b with_param
      | WP_App: ('a -> 'b) with_param * 'a with_param -> 'b with_param
      | WP_Deferred: 'a Deferred.t with_param -> 'a with_param
      | WP_Connection: Git.connection with_param

    type exists_param =
      | Exists_param: ('a,'b) param -> exists_param

    let params : exists_param String.Table.t = String.Table.create ()

    module ParamValue = struct
      module Id = Type_equal.Id
      module Uid = Type_equal.Id.Uid
      (** Same as Univ_map except we need merge function *)

      type elt = T : ('a,'b) param * 'a -> elt
      let sexp_of_elt (T(p,v)) =
        let s = Id.to_sexp p.id v in
        let n = String.sexp_of_t (Id.name p.id) in
        Sexp.List[n;s]
      let elt_of_sexp s =
        match s with
        | Sexp.List [n;s] -> begin
          let n = String.t_of_sexp n in
          match String.Table.find params n with
          | None -> invalid_argf "parameter %s unknown" n ()
          | Some (Exists_param p) ->
            match p.of_sexp with
            | None ->
              invalid_argf "The parameter %s doesn't have of_sexp given" n ()
            | Some f -> T(p, f s)
          end
        | _ -> invalid_arg "Bad sexp, pair waited"
      type t = elt Uid.Map.t
      let sexp_of_t m = [%sexp_of: elt list] (Uid.Map.data m)
      let t_of_sexp m = List.fold ~init:Uid.Map.empty
          ([%of_sexp: elt list] m)
          ~f:(fun acc (T(p,_) as v) -> Uid.Map.add acc
                 ~key:(Id.uid p.id) ~data:v)
      let empty = Uid.Map.empty
      let mem m p = Uid.Map.mem m (Id.uid p.id)
      let find (type b) t (key : (b,_) param) =
        match Map.find t (Id.uid key.id) with
        | None -> None
        | Some (T (key', value)) ->
          let Type_equal.T = Id.same_witness_exn key.id key'.id in
          Some (value : b)
      let find_def (type b) t (key : (b,_) param) =
        match Map.find t (Id.uid key.id) with
        | None -> key.default
        | Some (T (key', value)) ->
          let Type_equal.T = Id.same_witness_exn key.id key'.id in
          (value : b)
      let set t p v = Uid.Map.add t ~key:(Id.uid p.id) ~data:(T(p, v))
      let replace_by m m' =
        Uid.Map.merge m m'
          ~f:(fun ~key:_ ->
              function | `Left x | `Right x | `Both (_,x) -> Some x)
    end

    let const x = WP_Const x
    let ( !! ) x = WP_Const x
    let param x = WP_Param x
    let ( !? ) x = WP_Param x
    let ( $ ) f x = WP_App(f,x)
    let ( $? ) f x = WP_App(f,WP_Param x)
    let connection = WP_Connection
    let join_deferred x = WP_Deferred x
    let ( $! ) f x = WP_Deferred (WP_App(f,x))

    let mk_param'
        ~default ?(sexp_of=sexp_of_opaque) ?of_sexp ~to_option_hum
        ~cmdliner
        ?docv
        ?doc
        ~resolve ~unresolve name =
      let id = Univ_map.Key.create ~name sexp_of in
      let param = {default; id; of_sexp;
                   cmdliner =
                     Cmdliner.Arg.(value & cmdliner & info [name]
                                     ?docv ?doc);
                   to_option_hum;
                   resolve;
                   unresolve;
                  } in
      String.Table.add_exn params ~key:name ~data:(Exists_param param);
      param

    let mk_param
        ~default ?sexp_of ~of_sexp ~to_option_hum
        ~cmdliner ?docv ?doc name =
      mk_param'
        ~default ?sexp_of ~of_sexp ~to_option_hum
        ~cmdliner ?docv ?doc
        ~resolve:(const (fun _ b -> return b))
        ~unresolve:(fun b -> b) name

    let mk_param_string' ~default ?docv ?doc ~resolve ~unresolve name =
      mk_param' ~cmdliner:Cmdliner.Arg.(opt string default) ~default
        ?docv ?doc ~sexp_of:String.sexp_of_t ~of_sexp:String.t_of_sexp
        ~to_option_hum:(fun s -> sprintf "--%s=%s" name s)
        ~resolve ~unresolve
        name

    let mk_param_string ~default ?docv ?doc name =
      mk_param_string' ~default ?docv ?doc name
        ~resolve:(const (fun _ b -> return b))
        ~unresolve:(fun b -> b)

    let mk_param_string_list
        ?(default=[]) ?docv ?doc name =
      mk_param
        ~default
        ~cmdliner:Cmdliner.Arg.(opt_all string default)
        ?docv ?doc
        ~sexp_of:(List.sexp_of_t String.sexp_of_t)
        ~of_sexp:(List.t_of_sexp String.t_of_sexp)
        ~to_option_hum:(fun l ->
            String.concat (List.map ~f:(fun s -> sprintf "--%s=%s" name s) l))
        name


    let rec interp :
      type a. Git.connection -> ParamValue.t ->
      string list -> a with_param ->
      (a * string list) Deferred.t =
      fun c m acc -> function
        | WP_Const x -> return (x,acc)
        | WP_Deferred x ->
          interp c m acc x
          >>= fun (x,acc) ->
          x >>= fun x ->
          return (x,acc)
        | WP_App(f,x) ->
          interp c m acc f
          >>= fun (f,acc) ->
          interp c m acc x
          >>= fun (x,acc) ->
          return (f x,acc)
        | WP_Param p ->
          let a = ParamValue.find_def m p in
          (interp c m acc p.resolve)
          >>= fun (f,acc) ->
          f c a
          >>= fun a ->
          let s = p.to_option_hum (p.unresolve a) in
          return (a,s::acc)
        | WP_Connection -> return (c,acc)
  end

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

  type copts = { verb : Log.Level.t;  socket: string}


  let db_url_copy_file_md5sum = String.Table.create ()
  let add_copy_file_url kind checksum url =
    match kind with
    | `MD5 ->
      String.Table.add_multi db_url_copy_file_md5sum ~key:checksum ~data:url

  let mk_copy_file ~url ~checksum ~kind dst =
    List.iter url ~f:(add_copy_file_url kind checksum);
    Git.copy_file ~checksum ~kind dst

  type repo_param = Git.repo WP.with_param

  let db_repos : repo_param String.Map.t ref =
    ref String.Map.empty

  let add_repo name repo =
    db_repos := String.Map.add !db_repos ~key:name
        ~data:(WP.const repo)

  let add_repo_with_param name repo =
    db_repos := String.Map.add !db_repos ~key:name ~data:repo

  type query = Oci_Generic_Masters_Api.CompileGitRepo.Query.t
  type revspecs = WP.ParamValue.t
  module Oci_Log = struct
    type kind = Oci_Log.kind =
      | Standard | Error | Chapter | Command
      [@@deriving sexp, bin_io]
    type 'a data = 'a Oci_Log.data =
      | Std of kind * string
      | Extra of 'a
      | End of unit Or_error.t
      [@@deriving sexp, bin_io]
    type 'a line = 'a Oci_Log.line = {
      data : 'a data;
      time : Time.t;
    } [@@deriving sexp, bin_io]
    type t = Git.compile_and_tests
  end

  type log = Oci_Log.t Oci_Log.line

  type 'res compare_result =
    ('res,
     [ `Anomaly of Error.t
     | `BadResult of string ]) Result.t


  type ('x,'y,'res) all_result = {
    x: 'x array;
    y: 'y array;
    res: 'res compare_result array array;
    (** (x).(y) *)
  }

  type ('x,'y,'acc) compare' =
    Git.connection ->
    revspecs -> 'x -> 'y ->
    (revspecs * repo_param * ('acc -> log -> 'acc Deferred.t)) Deferred.t
  type ('x,'y) compare =
    Git.connection ->
    revspecs -> 'x -> 'y ->
    (revspecs * repo_param) Deferred.t
  type ('acc,'res,'x,'y) compareN = {
    fold_init: 'acc;
    fold_end: 'acc -> 'res compare_result;
    fold: ('x,'y,'acc) compare';
    sexp_of_x: 'x -> Sexp.t;
    x_of_sexp: Sexp.t -> 'x;
    sexp_of_y: 'y -> Sexp.t;
    y_of_sexp: Sexp.t -> 'y;
    analyse: ('x,'y,'res) all_result -> ('x,'y,float) all_result;
    timeout: float;
  }
  type exists_compare =
    | CompareN: ('acc,'res,'x,'y) compareN -> exists_compare
  let db_compare = ref String.Map.empty

  let add_compare name compareN =
    db_compare :=
      String.Map.add !db_compare ~key:name
        ~data:(CompareN(compareN))

  let used_interp_repos c revspecs toprint root repos =
    let open! Oci_Generic_Masters_Api.CompileGitRepo.Query in
    let seen = String.Table.create () in
    let new_repos = ref String.Map.empty in
    let toprint = ref toprint in
    let rec aux name =
      if String.Table.mem seen name then Deferred.unit
      else
        match String.Map.find repos name with
        | None -> raise (MissingRepo name)
        | Some repo ->
          WP.interp c revspecs [] repo
          >>= fun (repo,acc) ->
          toprint := List.fold acc ~init:!toprint ~f:String.Set.add;
          new_repos := String.Map.add !new_repos ~key:name ~data:repo;
          Deferred.List.iter repo.deps ~how:`Parallel ~f:aux
    in
    aux root
    >>= fun () ->
    return (!new_repos,!toprint)

  type repo = String.t

  type create_query_hook =
    connection:Git.connection ->
    root:string ->
    revspecs:WP.ParamValue.t ->
    (repo * WP.ParamValue.t) Async.Std.Deferred.t

  let default_create_query_hook : create_query_hook =
    fun ~connection:_ ~root ~revspecs ->
      Deferred.return (root, revspecs)

  let create_query
      ?(info_level=`Info)
      (create_query_hook:create_query_hook)
      rootfs revspecs repo repos connection =
    Rpc.Rpc.dispatch_exn (Oci_Data.rpc Oci_Rootfs_Api.find_rootfs)
      connection.Git.socket rootfs
    >>= fun rootfs ->
    let rootfs = (Or_error.ok_exn rootfs) in
    Log.Global.printf ~level:info_level "Check the revspecs:";
    create_query_hook ~connection ~root:repo ~revspecs
    >>= fun (root, revspecs) ->
    used_interp_repos connection revspecs String.Set.empty root repos
    >>= fun (repos,toprint) ->
    Deferred.Map.iteri repos
      ~how:`Parallel
      ~f:(fun ~key:_ ~data:repo ->
          Deferred.List.iter
            repo.Oci_Generic_Masters_Api.CompileGitRepo.Query.cmds
            ~f:(function
                | `LinkFile {Oci_Generic_Masters_Api.
                              CompileGitRepoRunner.checksum;kind} -> begin
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
                                error "Download error: %s"
                                  (Error.to_string_hum e);
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
                | _ -> Deferred.unit
              )
        )
    >>= fun () ->
    let query = Git.repos
        ~name:root
        ~rootfs
        ~repos
    in
    Log.Global.printf ~level:info_level
      "configuration: --rootfs %s %s"
      (Oci_Rootfs_Api.Rootfs_Id.to_string rootfs.id)
      (String.concat ~sep:" " (String.Set.to_list toprint));
    return query

  let run cq_hook rootfs revspecs (repo:string) connection =
    create_query cq_hook rootfs revspecs repo !db_repos connection
    >>= fun query ->
    let fold acc = function
      | `Ok (`CmdStart(_)) -> acc
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
                | `LinkFile _ -> true
                | `Exec _ -> false) data.cmds;
              };)
    in
    let query = { query with repos }
    in
    exec Oci_Generic_Masters_Api.XpraGitRepo.rpc query
      Oci_Generic_Masters_Api.CompileGitRepo.Query.sexp_of_t
      Oci_Generic_Masters_Api.XpraGitRepo.Result.pp socket

  let gen_fold_compare fold_analyse (acc,acc_analyse) r =
    let acc = match r with
    | {Oci_Log.data=Oci_Log.Std (_,line);_} ->
      debug "[Std] %s" line;
      acc
    | {Oci_Log.data=Oci_Log.Extra r} ->
      debug
        "[Exec] %s@."
        (Oci_pp.string_of
           Oci_Generic_Masters_Api.CompileGitRepo.Result.pp r);
      acc
    | {Oci_Log.data=Oci_Log.End r} -> begin
      match r with
      | Error r ->
        debug "[End] error: %s"
          (Sexp.to_string_hum (Error.sexp_of_t r));
      | Ok () -> ()
      end;
      r
    in
    fold_analyse acc_analyse r
    >>= fun acc_analyse ->
    return (acc,acc_analyse)

  let compare_exec_one compareN cq_hook connection rootfs revspecs x y =
    compareN.fold connection revspecs x y
    >>= function (revspecs, git_repo, fold_analyse) ->
      let repo_compare = "Oci_Client.compare" in
      let repos = String.Map.add !db_repos ~key:repo_compare
          ~data:git_repo in
      create_query
        ~info_level:`Debug
        cq_hook rootfs revspecs repo_compare
        repos connection
      >>= fun query ->
      Rpc.Pipe_rpc.dispatch_exn
        (Oci_Data.log Oci_Generic_Masters_Api.CompileGitRepo.rpc)
        connection.Git.socket query
      >>= fun (p,_) ->
      Pipe.fold p
        ~init:(Error (Error.of_string "incomplete log"), compareN.fold_init)
        ~f:(gen_fold_compare fold_analyse)
      >>= fun (complete,res) ->
      let res = compareN.fold_end res in
      return (match complete with
          | Ok () -> res
          | Error e -> Error (`Anomaly e))

  let compare_run_one cq_hook rootfs revspecs
      (x_input:string) (y_input:string) bench
      connection =
    match String.Map.find_exn !db_compare bench with
    | CompareN compareN ->
      let x = Sexp.of_string_conv_exn x_input compareN.x_of_sexp in
      let y = Sexp.of_string_conv_exn y_input compareN.y_of_sexp in
      compare_exec_one compareN cq_hook connection rootfs revspecs x y
      >>= function
      | Error (`BadResult s) ->
        error "[BadResult] error: %s" s;
        return `Error
      | Error (`Anomaly r) ->
        error "[Anomaly] error: %s"
          (Sexp.to_string_hum (Error.sexp_of_t r));
        return `Error
      | Ok _ ->
        info "Done";
        return `Ok

  let compare cq_hook rootfs revspecs
      (x_input:Oci_Filename.t) (y_input:Oci_Filename.t) bench
      outputs summation connection =
    let load_sexps input sexp =
      Reader.load_sexps input sexp
      >>= function
      | Ok r -> return r
      | Error err ->
        error "Error during parsing of %s:\n%s"
          input (Error.to_string_hum err);
        Shutdown.exit 1
    in
    match String.Map.find_exn !db_compare bench with
    | CompareN compareN ->
      load_sexps x_input compareN.x_of_sexp
      >>= fun lx ->
      begin match summation with
        | `Two when List.length lx <> 2 ->
          error "When using --compare-two, --x-input should \
                 contain only two elements";
          Shutdown.exit 1
        | _ -> Deferred.unit
      end
      >>= fun () ->
      load_sexps y_input compareN.y_of_sexp
      >>= fun ly ->
      Deferred.List.map
        ~how:`Parallel
        lx ~f:(fun x ->
            Deferred.List.map
              ~how:`Parallel ly
              ~f:(compare_exec_one
                    compareN cq_hook connection rootfs revspecs x)
          )
      >>= fun results ->
      let results = {
        x = Array.of_list lx;
        y = Array.of_list ly;
        res = Array.of_list_map ~f:Array.of_list results
      } in
      let results = compareN.analyse results in
      let lost_runs =
        Array.exists results.res ~f:(Array.exists ~f:Result.is_ok) in
      let results =
        List.map2_exn ~f:(fun x a ->
            (x,Array.to_list (Array.map ~f:Result.ok a)))
          (Array.to_list results.x)
          (Array.to_list results.res) in
      let filler = lazy begin
        let results =
          List.map ~f:(fun (x,l) -> Sexp.to_string (compareN.sexp_of_x x),l)
            results
        in
        let filter_results r =
          List.map r ~f:(fun (x,l) -> (x, List.filter_opt l))
        in
        match summation,results with
        | `Timeout,_ ->
          Gnuplot.compute_datas_timeout
            compareN.timeout(filter_results results)
        | `Sort,_ -> Gnuplot.compute_datas_sort
                       compareN.timeout(filter_results results)
        | `Two,[a;b] -> Gnuplot.compute_datas_two
                          compareN.timeout a b
        | `Two, _ -> assert false (* The previous test must forbid this case *)
      end in
      Deferred.List.iter outputs ~f:(function
          | `Sexp filename ->
            let sexp =
              List.sexp_of_t
                (sexp_of_pair compareN.sexp_of_x
                   (List.sexp_of_t (Option.sexp_of_t Float.sexp_of_t))) in
            Writer.save_sexp ~hum:true filename (sexp results)
          | `Gnuplot filename ->
            Writer.open_file filename
            >>= fun writer ->
            (Lazy.force filler) (`Svg("foobar.svg")) (Writer.pipe writer)
            >>= fun () ->
            Writer.close writer
          | `Png filename ->
            Gnuplot.call_gnuplot ((Lazy.force filler) (`Png(640,480,filename)))
          | `Svg filename ->
            Gnuplot.call_gnuplot ((Lazy.force filler) (`Svg(filename)))
          | `Qt ->
            Gnuplot.call_gnuplot ((Lazy.force filler) `Qt)
        )
      >>= fun () ->
      Deferred.return (if lost_runs then `Error else `Ok)

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
      ~heartbeat_config:Oci_Artefact_Api.heartbeat_config
      ~connection_state:(fun _ -> ())
      ~description:(Info.of_string "Client <-> Master")
      reader writer
    >>= fun socket ->
    let socket = Result.ok_exn socket in
    cmd {Git.socket;
         revspec_cache=Hashtbl.Poly.create ();
         download_cache=Hashtbl.Poly.create ()}
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
      let def =
        Oci_Filename.make_absolute Oci_Version.default_oci_data "oci.socket" in
      Arg.(value & opt file def & info ["socket"]
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
    String.Table.fold WP.params
      ~init:Term.(const init)
      ~f:Cmdliner.Term.(fun ~key:_ ~data acc ->
          match data with
          | WP.Exists_param ({cmdliner} as p) ->
            const (fun a m -> WP.ParamValue.set m p a) $ cmdliner $ acc
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
           (cmdliner_revspecs WP.ParamValue.empty) $ repo),
     Term.info "run" ~doc ~sdocs:copts_sect ~man;
     Term.(const xpra $ create_query_hook $ rootfs $
           (cmdliner_revspecs WP.ParamValue.empty) $ repo),
     Term.info "xpra" ~doc ~sdocs:copts_sect ~man]

  let compare_cmd create_query_hook =
    let bench =
      let benchs = String.Map.keys !db_compare in
      let benchs_enum = List.map ~f:(fun x -> (x,x)) benchs in
      Arg.(required & pos 0 (some (enum benchs_enum)) None & info []
             ~docv:"COMPARE_NAME"
             ~doc:("Run the comparison $(docv). \
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
              `Two, info ["compare-two"]
                ~doc:"Compare each run individually";
             ])
    in
    let doc = "run a specific benchmark" in
    let man = [
      `S "DESCRIPTION";
      `P "Run the benchmark for comparing the x with the y\
          (TODO: find a name for x and y)."] @ help_secs
    in
    Term.(const compare $
          create_query_hook $ rootfs $
          (cmdliner_revspecs WP.ParamValue.empty) $ x_input $ y_input $ bench
          $ outputs $ summation),
    Term.info "compare" ~doc ~sdocs:copts_sect ~man

  let compare_run_one_cmd create_query_hook =
    let bench =
      let benchs = String.Map.keys !db_compare in
      let benchs_enum = List.map ~f:(fun x -> (x,x)) benchs in
      Arg.(required & pos 0 (some (enum benchs_enum)) None & info []
             ~docv:"COMPARE_NAME"
             ~doc:("Run the comparison $(docv). \
                    Possible values: " ^
                   (String.concat ~sep:", " benchs) ^ "."))
    in
    let x_input =
      Arg.(required & opt (some string) None & info ["x-sexp"]
             ~docv:"X"
             ~doc:"sexp for one x variable")
    in
    let y_input =
      Arg.(required & opt (some string) None & info ["y-sexp"]
             ~docv:"Y"
             ~doc:"sexp for one x variable")
    in
    let doc = "run a specific benchmark" in
    let man = [
      `S "DESCRIPTION";
      `P "Run the benchmark for comparing the x with the y."] @ help_secs
    in
    Term.(const compare_run_one $
          create_query_hook $ rootfs $
          (cmdliner_revspecs WP.ParamValue.empty) $ x_input $ y_input $ bench),
    Term.info "compare_run_one" ~doc ~sdocs:copts_sect ~man

  let default_cmd ?version ?doc name =
    let man = help_secs in
    Term.(ret (const (`Help (`Pager, None)))),
    Term.info name ?version ?doc ~man

  let def_cmds_with_connection create_query_hook =
    [list_rootfs_cmd; create_rootfs_cmd;
     add_package_cmd;
     download_rootfs_cmd]@
    (if String.Map.is_empty !db_compare
     then [] else [compare_cmd create_query_hook;
                   compare_run_one_cmd create_query_hook])@
    (run_xpra_cmd create_query_hook)

  let def_cmds_without_connection =
    [list_download_rootfs_cmd]

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

  (* let git_clone ?dir url = *)
  (*   if not (String.Table.mem url_to_default_revspec url) *)
  (*   then invalid_argf *)
  (*       "The url have not been registered with \ *)
  (*        add_default_revspec_for_url: %s" url (); *)
  (*   Git.git_clone ?dir ~url dumb_commit *)

  (* let git_copy_file ~src ~dst url = *)
  (*   if not (String.Table.mem url_to_default_revspec url) *)
  (*   then invalid_argf *)
  (*       "The url have not been registered with \ *)
  (*        add_default_revspec_for_url: %s" url (); *)
  (*   Git.git_copy_file ~src ~dst ~url dumb_commit *)

  let mk_revspec_param ?(revspec="master") ~url name =
    WP.mk_param_string'
      ~default:revspec
      name
      ~docv:"REVSPEC"
      ~doc:(sprintf "indicate which revspec of %s to use." name)
      ~resolve:(WP.const
                  (fun connection revspec ->
                     Git.commit_of_revspec_exn ~url ~revspec connection
                  ))
      ~unresolve:Oci_Common.Commit.to_string

  let mk_repo ?revspec ~url ~deps ~cmds ?(tests=[]) name =
    let revspec_param = mk_revspec_param ?revspec ~url name in
    add_repo_with_param name
      WP.(const (fun commit ->
          Git.repo
            ~deps
            ~cmds:((Git.git_clone ~url commit)::cmds)
            ~tests
            ()) $? revspec_param);
    name, revspec_param

  let mk_compare'
      ~(repos:('x,'y,'acc) compare')
      ~x_of_sexp ~sexp_of_x ~y_of_sexp ~sexp_of_y
      ~fold_init
      ~fold_end
      ~analyse
      ~timeout
      (name:string) =
    add_compare
      name
      {
        fold_init;
        fold_end;
        fold = (fun conn revspecs x y ->
            repos conn revspecs x y
            >>= fun (revspecs,repo,fold_analyse) ->
            (* set save_artefact = false *)
            return
              (revspecs,
               WP.(const (fun repo ->
                   {repo with
                    Oci_Generic_Masters_Api.CompileGitRepo.Query.
                     save_artefact = false}) $ repo),
               fold_analyse
              )
          );
        x_of_sexp;
        sexp_of_x;
        y_of_sexp;
        sexp_of_y;
        analyse;
        timeout;
      }

  let mk_compare
      ~repos
      ~x_of_sexp ~sexp_of_x ~y_of_sexp ~sexp_of_y
      ~analyse
      ~timeout
      name =
    let fold_init = Error (Error.of_string "no command run") in
    let fold_analyse acc =
      function
      | {Oci_Log.data=Oci_Log.Std (_,_)} ->
        return acc
      | {Oci_Log.data=Oci_Log.Extra (`Cmd(_,result,time))} ->
        return (Ok (result,time))
      | {Oci_Log.data=Oci_Log.Extra _} ->
        return acc
      | {Oci_Log.data=Oci_Log.End (Error err)} ->
        return (Error err)
      | {Oci_Log.data=Oci_Log.End (Ok ())} ->
        return acc
    in
    let fold_end = function
      | Error r -> Error (`Anomaly r)
      | Ok (result,time) -> analyse result time in
    let analyse r = r in
    let repos conn revspecs x y =
      repos conn revspecs x y
      >>= fun (revspec,repo) ->
      return (revspec,repo,fold_analyse)
    in
    mk_compare'
      ~repos ~fold_end
      ~x_of_sexp ~sexp_of_x ~y_of_sexp ~sexp_of_y
      ~fold_init ~analyse ~timeout
      name

  type compare_many = string * WP.ParamValue.t [@@ deriving sexp]

  let mk_compare_many_using_revspecs'
      ~repos ~y_of_sexp ~sexp_of_y ~fold_init ~fold_end ~analyse ~timeout name =
    mk_compare'
      ~x_of_sexp:compare_many_of_sexp
      ~sexp_of_x:sexp_of_compare_many
      ~y_of_sexp ~sexp_of_y
      ~repos:(fun conn revspecs (x_name,x) y ->
          let revspecs = WP.ParamValue.replace_by revspecs x in
          let repo =
            match List.find_map repos
              ~f:(fun (x,d) ->
                  if String.equal x_name x
                  then Some d else None)
            with
            | None -> invalid_argf "name of tool %s unknown" x_name ()
            | Some repo -> repo
          in
          repo conn y
          >>= fun (repo,analyse) ->
          return (revspecs, repo, analyse)
        )
      ~fold_init
      ~fold_end
      ~analyse
      ~timeout
      name

  let mk_compare_many_using_revspecs
      ~repos ~y_of_sexp ~sexp_of_y ~analyse ~timeout name =
    mk_compare
      ~x_of_sexp:[%of_sexp: (string * WP.ParamValue.t)]
      ~sexp_of_x:[%sexp_of: (string * WP.ParamValue.t)]
      ~y_of_sexp ~sexp_of_y
      ~repos:(fun conn revspecs (x_name,x) y ->
          let revspecs = WP.ParamValue.replace_by revspecs x in
          let repo = List.find_map_exn repos
              ~f:(fun (x,d) ->
                  if String.equal x_name x
                  then Some d else None)
          in
          repo conn y
          >>= fun repo ->
          return (revspecs, repo)
        )
      ~analyse
      ~timeout
      name


  module Predefined = struct
    open Git
    let ocaml_url = "https://github.com/ocaml/ocaml.git"
    let ocaml_cmds commit configure_opt = [
      Git.git_clone ~url:ocaml_url commit;
      run "./configure" configure_opt;
      make ["world.opt"];
      make ["install"];
      run "mkdir" ["-p";"/usr/local/lib/ocaml/site-lib/stublibs/"];
      run "touch" ["/usr/local/lib/ocaml/site-lib/stublibs/.placeholder"];
      run "sh" ["-c";"echo /usr/local/lib/ocaml/site-lib/stublibs/ >> \
                      /usr/local/lib/ocaml/ld.conf"]
    ]
    let ocaml_configure =
      WP.mk_param_string_list
        ~docv:"ARG"
        ~doc:"Determine the argument to give to ocaml configure"
        "ocaml-configure"

    let ocaml_revspec =
      mk_revspec_param
        ~revspec:"bdf3b0fac7dd2c93f80475c9f7774b62295860c1"
        ~url:ocaml_url
        "ocaml"

    let ocaml =
      add_repo_with_param "ocaml"
        WP.(const (fun commit configure_opt ->
            Git.repo
              ~deps:[]
              ~cmds:(ocaml_cmds commit configure_opt)
              ())
            $? ocaml_revspec
            $? ocaml_configure);
      "ocaml"

    let ocamlfind, ocamlfind_revspec = mk_repo
        "ocamlfind"
        ~url:"https://gitlab.camlcity.org/gerd/lib-findlib.git"
        ~deps:[ocaml]
        ~cmds:[
          run "./configure" [];
          make ["all"];
          make ["opt"];
          make ["install"];
        ]

    let ocamlbuild,ocamlbuild_revspec = mk_repo
        "ocamlbuild"
        ~url:"https://github.com/ocaml/ocamlbuild.git"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          (* don't compile and install ocamlbuild if it is already installed
              (before ocaml 4.03) *)
          run "sh" ["-c"; "which ocamlbuild || make"];
          run "sh" ["-c"; "which ocamlbuild || make install"];
        ]

    let zarith,zarith_revspec = mk_repo
        "ZArith"
        ~url:"https://github.com/bobot/zarith.git"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          run "./configure" [];
          make [];
          make ["install"];
        ]

    let xmllight,xmllight_revspec = mk_repo
        "xml-light"
        ~url:"https://github.com/ncannasse/xml-light.git"
        ~revspec:"2.4"
        ~deps:[ocaml;ocamlfind]
        ~cmds:[
          make ["install_ocamlfind"]
        ]

    let camlp4,camlp4_revspec = mk_repo
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

    let lablgtk,lablgtk_revspec = mk_repo
        "lablgtk"
        ~url:"https://forge.ocamlcore.org/anonscm/git/lablgtk/lablgtk.git"
        ~revspec:"28290b0ee79817510bbc908bc733e80258aea7c1"
        ~deps:[ocaml;ocamlfind;camlp4]
        ~cmds:[
          run "./configure" [];
          make ["world"];
          make ~env:(`Extend ["OCAMLFIND_LDCONF","ignore"]) ["install"];
        ]

    let ocamlgraph,ocamlgraph_revspec = mk_repo
        "ocamlgraph"
        ~url:"https://github.com/backtracking/ocamlgraph.git"
        ~deps:[ocaml;ocamlfind;lablgtk]
        ~cmds:[
          run "autoconf" [];
          run "./configure" [];
          make [];
          make ["install-findlib"];
        ]

    let cppo,cppo_revspec = mk_repo
        "cppo"
        ~url:"https://github.com/mjambon/cppo.git"
        ~deps:[ocaml;ocamlfind;ocamlbuild]
        ~cmds:[
          make [];
          make ["install"];
        ]

    let camomile,camomile_revspec =
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
        ~deps:[ocaml;ocamlfind;ocamlbuild]
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
      "ounit"

    let cryptokit,cryptokit_revspec = mk_repo
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
