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

type 'r t = {
  connection : Rpc.Connection.t;
  log : 'r Oci_Log.line Pipe.Writer.t;
}

(** Enter inside the namespace *)
let () =
  Caml.Unix.chroot ".";
  Caml.Unix.chdir "/"

let start ~implementations =
  begin
    let implementations =
      (Rpc.Rpc.implement Oci_Artefact_Api.rpc_stop_runner
         (fun _ () -> shutdown 0; return ()))::
       implementations in
    let implementations =
      Rpc.Implementations.create_exn
        ~on_unknown_rpc:`Raise
        ~implementations in
    let named_pipe = Sys.argv.(1) in
    Reader.open_file (named_pipe^".in")
    >>> fun reader ->
    Writer.open_file (named_pipe^".out")
    >>> fun writer ->
    Rpc.Connection.create
      ~heartbeat_config:Oci_Artefact_Api.heartbeat_config
      ~implementations
      ~connection_state:(fun c -> c)
      ~description:(Info.createf "Runner (%s) <-> Master" Sys.executable_name)
      reader writer
    >>> fun conn ->
    let conn = Result.ok_exn conn in
    Shutdown.at_shutdown (fun () ->
        Rpc.Connection.close conn
        >>= fun () ->
        Reader.close reader;
        >>= fun () ->
        Writer.close writer
      )
  end;
  Scheduler.go ()

let implement data f =
  Rpc.Pipe_rpc.implement
    (Oci_Data.log data)
    (fun connection q ->
       let reader = Pipe.init (fun writer ->
           Monitor.try_with_or_error ~here:[%here]
             (fun () -> f {connection;log=writer} q)
           >>= fun res ->
           Oci_Log.write_and_close writer res
         ) in
       Deferred.Or_error.return reader
    )

exception StopQuery

let implement_unit data f =
  Rpc.Pipe_rpc.implement
    (Oci_Data.log data)
    (fun connection q ->
       let reader = Pipe.init (fun writer ->
           Monitor.try_with
             ~here:[%here]
             ~name:"Oci_Runner.implement_*"
             ~run:`Now
             ~rest:`Log
             (fun () ->
                f {connection;log=writer} q
             )
           >>= fun res ->
           let res =
             match res with
             | Ok () -> Ok ()
             | Error exn ->
               match Monitor.extract_exn exn with
               | StopQuery -> Ok ()
               | _ -> Or_error.of_exn exn
           in
           Oci_Log.close_writer writer res
         )
in
Deferred.Or_error.return reader
    )

let write_log kind t fmt =
  Printf.ksprintf (fun s ->
      s
      |> String.split_lines
      |> List.iter
        ~f:(fun line -> Pipe.write_without_pushback t.log
               (Oci_Log.line kind line))
    ) fmt

let std_log t fmt = write_log Oci_Log.Standard t fmt
let err_log t fmt = write_log Oci_Log.Error t fmt
let cmd_log t fmt = write_log Oci_Log.Command t fmt
let cha_log t fmt = write_log Oci_Log.Chapter t fmt
let data_log t d =
  Pipe.write_without_pushback t.log
    (Oci_Log.data d)

type artefact = Oci_Common.Artefact.t [@@deriving sexp, bin_io]

let create_artefact ?(rooted_at="/") ?(prune=[]) ?(only_new=true) t ~dir =
  assert (Oci_Filename.is_subdir ~parent:rooted_at ~children:dir);
  cmd_log t "Create artefact %s" dir;
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_create
    t.connection {prune;src=dir;rooted_at;only_new}
let link_artefact t ?(user=Oci_Common.Root) src ~dir =
  cmd_log t "Link artefact %s to %s" (Oci_Common.Artefact.to_string src)dir;
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_link_to
    t.connection (user,src,dir)
let copy_artefact t ?(user=Oci_Common.Root) src ~dir =
  cmd_log t "Copy artefact %s to %s" (Oci_Common.Artefact.to_string src)dir;
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_copy_to
    t.connection (user,src,dir)

let get_internet t =
  cmd_log t "Get internet";
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_get_internet
    t.connection ()

let give_external_access t src =
  cmd_log t "Give access to %s" src;
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_give_external_access
    t.connection src

let get_proc t requested =
  if not (0 < requested) then
    invalid_argf "get_proc: requested proc (%i) should be positive"
      requested ();
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_get_or_release_proc
    t.connection requested

let release_proc t released =
  if not (0 < released) then
    invalid_argf "get_proc: released proc (%i) should be positive"
      released ();
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_get_or_release_proc
    t.connection (-released)
  >>= fun _ ->
  Deferred.unit

let get_release_proc t requested f =
  begin
    if 1 < requested
    then get_proc t (requested-1)
    else return 0
  end
  >>= fun got ->
  Monitor.protect ~here:[%here]
    ~finally:(fun () ->
        if 1 < got
        then release_proc t got
        else return ())
    (fun () -> f (got+1))

let dispatch t d q =
  cmd_log t "Dispatch %s" (Oci_Data.name d);
  Rpc.Rpc.dispatch (Oci_Data.rpc d) t.connection q
  >>= fun r ->
  return (Or_error.join r)

let dispatch_exn t d q =
  cmd_log t "Dispatch %s" (Oci_Data.name d);
  Rpc.Rpc.dispatch_exn (Oci_Data.rpc d) t.connection q
  >>= fun r ->
  return (Or_error.ok_exn r)

let process_log t p =
  let send_to_log t kind reader =
    let reader = Reader.lines reader in
    Pipe.transfer ~f:(fun line -> Oci_Log.line kind line) reader t.log in
  Deferred.all_unit [
    send_to_log t Oci_Log.Standard (Process.stdout p);
    send_to_log t Oci_Log.Error (Process.stderr p);
  ]


let print_cmd prog args =
  prog ^ " " ^ (String.concat ~sep:", " args)

type 'a process_create
  =  ?env         : Process.env
  -> ?working_dir : string
  -> prog         : string
  -> args         : string list
  -> unit
  -> 'a Deferred.t

let process_create t ?env ?working_dir ~prog ~args () =
  Deferred.Or_error.bind
    (Process.create ?working_dir ?env ~prog ~args ())
    (fun p ->
       process_log t p
       >>= fun () ->
       Deferred.Or_error.return p)

exception CommandFailed

let run t ?env ?working_dir ~prog ~args () =
  cmd_log t "%s" (print_cmd prog args);
  process_create t ?working_dir ?env ~prog ~args ()
  >>= fun p ->
  let p = Or_error.ok_exn p in
  Process.wait p
  >>= fun r ->
  match r with
  | Core_kernel.Std.Result.Ok () -> return r
  | Core_kernel.Std.Result.Error _ as error ->
    err_log t "Command %s failed: %s"
      (print_cmd prog args)
      (Unix.Exit_or_signal.to_string_hum error);
    return r

let run_exn t ?env ?working_dir ~prog ~args () =
  run t ?working_dir ?env ~prog ~args ()
  >>= function
  | Core_kernel.Std.Result.Ok () -> return ()
  | Core_kernel.Std.Result.Error _ ->
    raise CommandFailed

exception TimeError

let run_timed t ?timelimit ?env ?working_dir ~prog ~args () =
  cmd_log t "%s" (print_cmd prog args);
  let tmpfile = Filename.temp_file "time" ".sexp" in
  let args = "--output"::tmpfile::"--quiet"::"--format"::
             "((cpu_kernel %Ss)(cpu_user %Us)(wall_clock %es))"::
             prog::args
  in
  let prog = "/usr/bin/time" in
  process_create t ?working_dir ?env ~prog ~args ()
  >>= fun p ->
  let p = Or_error.ok_exn p in
  let w = Process.wait p in
  begin
    match timelimit with
    | None -> Deferred.unit
    | Some timelimit ->
      Deferred.choose [
        Deferred.choice w (fun _ -> `Done);
        Deferred.choice (after timelimit) (fun _ -> `Timeout);
      ]
      >>= function
      | `Timeout ->
        ignore (Signal.send Signal.kill (`Pid (Process.pid p)));
        Deferred.unit
      | `Done -> Deferred.unit
  end
  >>= fun () ->
  w
  >>= fun r ->
  Reader.file_contents tmpfile
  >>= fun content ->
  Sys.remove tmpfile
  >>= fun () ->
  let timed = Sexp.of_string_conv
      (String.strip content)
      Oci_Common.Timed.t_of_sexp in
  match r, timed with
    | _, `Error (exn,_) ->
    let error = Error.of_exn exn in
    err_log t "time output parsing failed executing %s: %s"
      (print_cmd prog args)
      (Error.to_string_hum error);
    raise TimeError
  | Result.Ok (), `Result timed -> return (r,timed)
  | (Core_kernel.Std.Result.Error (`Exit_non_zero i) as r,`Result timed)
    when i < 128 -> return (r,timed)
  | (Core_kernel.Std.Result.Error (`Exit_non_zero i),`Result timed)
    -> return (Core_kernel.Std.Result.Error
                 (`Signal (Signal.of_system_int (i-128))),
               timed)
  | (Core_kernel.Std.Result.Error (`Signal _),_) -> raise TimeError

let run_timed_exn t ?env ?working_dir ~prog ~args () =
  run_timed t ?working_dir ?env ~prog ~args ()
  >>= function
  | (Result.Ok (), timed) -> return timed
  | _ -> raise CommandFailed

let git_clone_id = ref (-1)
let git_clone_dir = "/oci/git_clone/"
(* We put the clone in a safe repository (on the same filesystem)
    so that dst can be on a tmpfs
 *)
let git_clone t ?(user=Oci_Common.Root) ~url ~dst ~commit =
  incr git_clone_id;
  let id = !git_clone_id in
  let safe_dir = Oci_Filename.make_absolute git_clone_dir (string_of_int id) in
  Unix.mkdir ~p:() dst
  >>= fun () ->
  Unix.mkdir ~p:() safe_dir
  >>= fun () ->
  cmd_log t "Git clone %s in %s" url safe_dir;
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_git_clone
    t.connection {url;dst=safe_dir;user;commit}
  >>= fun () ->
  Unix.symlink
    ~src:(Oci_Filename.make_absolute safe_dir ".git")
    ~dst:(Oci_Filename.make_absolute dst ".git")
  >>= fun () ->
  run_exn t
    (* ~setuid:(Oci_Common.runner_user user).uid *)
    ~prog:"git"
    ~args:["-C";dst;"-c";"advice.detachedHead=false";"checkout";"--detach";
           Oci_Common.Commit.to_string commit] ()

let git_copy_file_id = ref (-1)
let git_copy_file_dir = "/oci/git_copy_file/"
let git_copy_file t ?(user=Oci_Common.Root) ~url ~src ~dst ~commit =
  cmd_log t "Git copy file %s from %s in %s" src url dst;
  incr git_copy_file_id;
  let id = !git_copy_file_id in
  let safe_file = Oci_Filename.make_absolute git_copy_file_dir
      (string_of_int id) in
  Unix.mkdir ~p:() git_copy_file_dir
  >>= fun () ->
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_git_copy_file
    t.connection {url;src;dst=safe_file;user;commit}
  >>= fun () ->
  Async_shell.run "mv" ["-f";safe_file;dst]


let get_file_id = ref (-1)
let get_file_dir = "/oci/get_file/"
let get_file t ~kind ~checksum ~dst =
  cmd_log t "Get file %s in %s" checksum dst;
  incr get_file_id;
  let id = !get_file_id in
  let safe_file = Oci_Filename.make_absolute get_file_dir
      (string_of_int id) in
  Unix.mkdir ~p:() get_file_dir
  >>= fun () ->
  Rpc.Rpc.dispatch_exn Oci_Artefact_Api.rpc_get_file
    t.connection {checksum;dst=safe_file;kind}
  >>= fun () ->
  Unix.symlink ~src:safe_file ~dst

let oci_version = Oci_Version.version
