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
open Oci_Common

open Log.Global

type used_procs =
  | SemiFreezed
  | Running of Int.t list * Int.t list * Int.t List.t List.t

let dumb_used_procs = SemiFreezed

type runners_data = {
  rootfs: Oci_Filename.t;
  mutable used_procs: used_procs;
  cgroup: string;
  mutable closed: bool;
}

type conf = {
  mutable last_artefact_id: Int.t;
  mutable last_runner_id: Int.t;
  conn_monitor : Rpc.Connection.t;
  binaries: Oci_Filename.t;
  storage: Oci_Filename.t;
  runners: Oci_Filename.t;
  permanent: Oci_Filename.t;
  log: Oci_Filename.t;
  git: Oci_Filename.t;
  wget: Oci_Filename.t;
  external_access: Oci_Filename.t;
  mutable last_external_access_id: Int.t;
  (** permanent storage for masters *)
  conf_monitor: Oci_Artefact_Api.artefact_api;
  api_for_runner: runners_data Rpc.Implementations.t;
  proc: Int.t List.t Pipe.Reader.t * Int.t List.t Pipe.Writer.t;
}

let gconf_ivar = Ivar.create ()
let gconf = Ivar.read gconf_ivar

type t = Oci_Common.Artefact.t

exception Directory_should_not_exists of Oci_Filename.t [@@deriving sexp]
exception Can't_copy_this_file of Oci_Filename.t [@@deriving sexp]

let get_conf () =
  Option.value_exn
    ~message:"Configuration can't be used before starting the `run` function"
    (Deferred.peek gconf)



let dir_of_artefact id =
  let dir = Oci_Filename.mk (Artefact.to_string id) in
  if Artefact.compare Artefact.empty id = 0
  then None
  else Some (Oci_Filename.make_absolute (get_conf ()).storage dir)

(*
let rec copydir
    ~hardlink ~prune_file ~prune_user
    ~chown:({User.uid;gid} as chown) src dst =
  (** This function run as superroot (like all masters),
      so it is root in its usernamespace *)
  Sys.file_exists_exn dst
  >>= fun dst_exist -> begin
    if dst_exist
    then return ()
    else
      Unix.mkdir ~p:() dst
      >>= fun () ->
      Unix.chown ~uid ~gid dst
  end
  >>= fun () ->
  Sys.ls_dir src
  >>= fun files ->
  Deferred.List.iter files ~f:(fun file ->
      let src' = Oci_Filename.make_absolute src file in
      let dst' = Oci_Filename.make_absolute dst file in
      if List.mem ~equal:Oci_Filename.equal prune_file src'
      then return ()
      else begin
        Unix.lstat src'
        >>= fun stat ->
        match (stat: Unix.Stats.t) with
        | { uid = 65534 } (* nobody *) | { gid = 65534 } (* nogroup *) ->
          return ()
        | {kind = `Directory} ->
          copydir ~hardlink ~prune_file ~prune_user ~chown src' dst'
        | {kind = (`File | `Link); uid; gid}
          when List.mem ~equal:User.equal prune_user {uid;gid}
          -> return ()
        | {kind = `File} ->
          Oci_Std.unlink_no_fail dst'
          >>= fun () ->
          if hardlink
          then Unix.link ~target:src' ~link_name:dst' ()
          else begin
            Async_shell.run "cp" ["-a";"--";src';dst']
            >>= fun () ->
            Unix.chown ~uid ~gid dst'
          end
        | {kind = `Link} ->
          Oci_Std.unlink_no_fail dst'
          >>= fun () ->
          Unix.readlink src'
          >>= fun tgt ->
          Unix.symlink ~src:tgt ~dst:dst'
          >>= fun () ->
          assert (not (Oci_Filename.is_relative dst'));
          if hardlink
          then return () (** keep superroot uid *)
          else
            In_thread.syscall_exn ~name:"fchownat" (fun () ->
                ExtUnix.Specific.fchownat
                  (ExtUnix.Specific.file_descr_of_int 0) (** dumb *)
                  dst' uid gid
                  [ExtUnix.Specific.AT_SYMLINK_NOFOLLOW]
              )
        | {kind = `Block|`Socket|`Fifo|`Char } ->
          error "Can't copy this file: %s bad type" src';
          raise (Can't_copy_this_file src')
      end
    )
*)

let copydir ~hardlink ~prune_file ~prune_user ~chown src dst =
  let prog =
    Oci_Filename.concat (get_conf ()).binaries
      (Oci_Filename.add_extension "Oci_Copyhard" "native") in
  Async_shell.run
    prog
    begin
      let add_options c l =
        List.concat (List.map ~f:(fun e -> [c;e]) l)
      in
      (if hardlink then ["--hardlink"] else [])@
      (add_options "--prune-file" prune_file)@
      (add_options "--prune-user" (List.map ~f:User.pp_chown prune_user))@
      ["--src";src;"--dst";dst;"--chown";
       User.pp_chown chown]
    end

let create ~prune ~rooted_at ~only_new ~src =
  assert (Oci_Filename.is_subdir ~parent:rooted_at ~children:src);
  let conf = get_conf () in
  conf.last_artefact_id <- conf.last_artefact_id + 1;
  let id = Artefact.of_int_exn conf.last_artefact_id in
  match dir_of_artefact id with
  | None -> assert false (** absurd: it can't be Artefact.empty *)
  | Some dst ->
    Sys.file_exists_exn (Oci_Filename.get dst)
    >>= fun b ->
    if b then raise (Directory_should_not_exists dst);
    let dst = Oci_Filename.reparent ~oldd:rooted_at ~newd:dst src in
    copydir
      ~hardlink:false
      ~prune_file:prune
      ~prune_user:(if only_new then [master_user Superroot] else [])
      ~chown:(master_user Superroot)
      src dst
    >>= fun () -> return id

let link_copy_to ~hardlink chown id dst =
  match dir_of_artefact id with
  | Some src -> copydir ~hardlink ~prune_file:[] ~prune_user:[] ~chown src dst
  | None -> Deferred.unit

let link_to_gen = link_copy_to ~hardlink:true
let copy_to_gen = link_copy_to ~hardlink:false

let link_to user = link_to_gen (Oci_Common.master_user user)
let copy_to user = copy_to_gen (Oci_Common.master_user user)

let is_available id =
  match dir_of_artefact id with
  | Some src -> Sys.file_exists_exn (Oci_Filename.get src)
  | None -> Deferred.return true

let remove_dir dir =
  Async_shell.run "rm" ["-rf";"--"; Oci_Filename.get dir]

let permanent_directory data =
  gconf
  >>= fun conf ->
  let dir =
    Oci_Filename.make_absolute conf.permanent
      (Oci_Filename.concat (Oci_Data.name data)
         (Int.to_string (Oci_Data.version data))) in
  Async_shell.run "mkdir" ["-p";"--";dir]
  >>= fun () ->
  return dir

let artifact_data_permanent_file conf =
  let dir = Oci_Filename.make_absolute conf.permanent "Oci_Artefact_Api" in
  Async_shell.run "mkdir" ["-p";"--";dir]
  >>= fun () ->
  return (Oci_Filename.concat dir "data")

let loader_artifact_data () =
  let conf = get_conf () in
  artifact_data_permanent_file conf
  >>= fun file ->
  Oci_Std.read_if_exists file Int.bin_reader_t
    (fun r -> conf.last_artefact_id <- r; return ())

let saver_artifact_data () =
  let conf = get_conf () in
  artifact_data_permanent_file conf
  >>= fun file ->
  Oci_Std.backup_and_open_file file
  >>= fun writer ->
  Writer.write_bin_prot writer Int.bin_writer_t conf.last_artefact_id;
  Writer.close writer

let invariant_procs = function
  | SemiFreezed -> true
  | Running (alloc,used,got) ->
    not (List.is_empty used &&
         List.is_empty alloc) &&
    not (List.exists ~f:List.is_empty got)

let give_to_global writer (l:Int.t List.t) =
  assert (not (List.is_empty l));
  Pipe.write_without_pushback writer l

let create_proc procs =
  let (_, writer) as pipes = Pipe.create () in
  List.iter procs ~f:(give_to_global writer);
  pipes

let create_used_procs () =
  let conf = get_conf () in
  let (reader, _) = conf.proc in
  Pipe.read reader
  >>= function
  | `Eof | `Ok [] -> assert false
  | `Ok (e::l) ->
  return (Running([e],l,[]), [e])

let get_proc local_procs nb =
  let conf = get_conf () in
  let (reader, writer) = conf.proc in
  let get_proc_in_global_pool nb hdgot got l =
    match Pipe.read_now' reader with
    | `Eof -> assert false
    | `Nothing_available ->
      Running([],hdgot,got), l
    | `Ok q ->
      let rec aux nb hdgot got l =
        match Queue.dequeue q with
        | None ->
          Running([],hdgot,got), l
        | Some e ->
          let e_len = List.length e in
          let nb' = nb - e_len in
          if nb' <= 0 then begin
            let e_used,e_alloc = List.split_n e nb in
            Queue.iter ~f:(give_to_global writer) q;
            Running(e_alloc,e_used,hdgot::got), (e_used@l)
          end
          else begin
            aux nb' e (hdgot::got) (e@l)
          end
      in
      aux nb hdgot got l
  in
  let rec get_proc_in_local_pool nb alloc used got l =
    if nb <= 0 then Running(alloc,used,got), l
    else
      match alloc with
      | [] ->
        get_proc_in_global_pool nb used got l
      | p::alloc ->
        get_proc_in_local_pool (nb-1) alloc (p::used) got (p::l)
  in
  assert (invariant_procs local_procs);
  match local_procs with
  | SemiFreezed -> begin
      Pipe.read reader
      >>= function
      | `Eof -> assert false
      | `Ok alloc ->
        assert (not (List.is_empty alloc));
        return (get_proc_in_local_pool nb alloc [] [] [])
    end
  | Running(alloc,used,got) ->
    return (get_proc_in_local_pool nb alloc used got [])


let release_proc local_procs nb =
  let conf = get_conf () in
  let (_, writer)  = conf.proc in
  let rec release_in_local_pool nb alloc used got l =
    if nb <= 0 then Running(alloc,used,got), l
    else
      match used with
      | [] ->
        begin match got with
          | [] ->
            if nb > 0 then debug "More cpu released than used!!";
            SemiFreezed, l
          | used::got ->
            give_to_global writer alloc;
            release_in_local_pool nb [] used got l
        end
      | p::used ->
        release_in_local_pool (nb-1) (p::alloc) used got (p::l)
  in
  assert (invariant_procs local_procs);
  match local_procs with
  | SemiFreezed ->
    if nb > 0 then debug "More cpu released than used!!";
    SemiFreezed, []
  | Running(alloc,used,got) ->
    release_in_local_pool nb alloc used got []

let release_all_proc local_procs =
  let conf = get_conf () in
  let (_, writer)  = conf.proc in
  assert (invariant_procs local_procs);
  match local_procs with
  | SemiFreezed -> SemiFreezed
  | Running(alloc,used,got) ->
    give_to_global writer (alloc@used);
    List.iter ~f:(give_to_global writer) got;
    SemiFreezed

let get_used_cpu local_procs =
  let conf = get_conf () in
  match local_procs with
  | SemiFreezed -> conf.conf_monitor.master_proc
  | Running(_,used,got) -> List.concat (used::got)

let update_cpuset d =
  let conf = get_conf () in
  Rpc.Rpc.dispatch_exn
    Oci_Artefact_Api.set_cpuset conf.conn_monitor {
    Oci_Artefact_Api.cgroup = d.cgroup;
    cpuset = get_used_cpu d.used_procs;
  }


(** {2 Management} *)
let masters =
  ref (Rpc.Implementations.create_exn
         ~implementations:[] ~on_unknown_rpc:`Close_connection)

module Direct_Master = struct
  type t =
    | DM: ('q,'r) Oci_Data.t * ('q -> 'r Oci_Log.reader) -> t
  let db = Type_equal.Id.Uid.Table.create ()

  let add_exn ~key:d ~data:f =
    Type_equal.Id.Uid.Table.add_exn
      db
      ~key:(Type_equal.Id.uid (Oci_Data.id d))
      ~data:(DM(d,f))

  let find_exn (type q) (type r) (d:(q,r) Oci_Data.t) :
    (q -> r Oci_Log.reader) =
    match Type_equal.Id.Uid.Table.find_exn
            db (Type_equal.Id.uid (Oci_Data.id d)) with
    | DM(d',f) ->
      match Type_equal.Id.same_witness_exn (Oci_Data.id d) (Oci_Data.id d') with
      | Type_equal.T -> f

end

let dispatch_master d q =
  let name = Oci_Data.name d in
  Monitor.try_with_join_or_error
    (fun () ->
       debug "%s called from a master" name;
       Monitor.try_with_join_or_error
         ~name (fun () ->
             let r = (Direct_Master.find_exn d) q in
             let r = Oci_Log.read r in
             let r = Pipe.filter_map r ~f:(function
                 | {Oci_Log.data = Oci_Log.Extra x} -> Some x
                 | _ -> None )
             in
             Pipe.read r
             >>= function
             | `Eof -> raise Oci_Data.NoResult
             | `Ok x ->
               Pipe.close_read r;
               return x))

let dispatch_master_exn d q =
  Deferred.Or_error.ok_exn
    (dispatch_master d q)

let dispatch_master_log d q =
  debug "%s called from a master" (Oci_Data.name d);
  (Direct_Master.find_exn d) q



let register_master
    ?(forget=fun _ -> Deferred.Or_error.return ())
    data (f: 'query -> 'result Oci_Log.reader) : unit =
  let name = (Printf.sprintf "Master %s" (Oci_Data.name data)) in
  let simple_rpc {rootfs} q =
    debug "%s called from %s" name rootfs;
    Monitor.try_with_join_or_error
      ~name (fun () ->
          let r = f q in
          let r = Oci_Log.read r in
          let r = Pipe.filter_map r ~f:(function
              | {Oci_Log.data = Oci_Log.Extra x} -> Some x
              | _ -> None )
          in
          Pipe.read r
          >>= function
          | `Eof -> raise Oci_Data.NoResult
          | `Ok x ->
            Pipe.close_read r;
            return x)
  in
  Direct_Master.add_exn
    ~key:data
    ~data:f;
  masters := Rpc.Implementations.add_exn !masters
      (Rpc.Rpc.implement (Oci_Data.rpc data) simple_rpc);
  masters := Rpc.Implementations.add_exn !masters
      (Rpc.Pipe_rpc.implement (Oci_Data.log data)
         (fun {rootfs} q ~aborted:_ ->
            debug "%s log called from %s" name rootfs;
            Monitor.try_with_or_error ~name
              (fun () -> return (Oci_Log.read (f q)))
         ));
  let forget _ q = Monitor.try_with_join_or_error (fun () -> forget q) in
  masters := Rpc.Implementations.add_exn !masters
      (Rpc.Rpc.implement (Oci_Data.forget data) forget);
  ()


let savers = Stack.create ()

let register_saver ~name ~loader ~saver =
  let loader () =
    Monitor.protect ~name:("Loader:"^name) loader ~finally:return in
  let saver  () =
    Monitor.protect ~name:("Saver:" ^name) saver ~finally:return in
  Stack.push savers (loader,saver)

let save =
  let sequence = Sequencer.create ~continue_on_error:true () in
  let f () =
    info "Save masters data";
    savers
    |> Stack.fold ~f:(fun acc (_,f) -> f ()::acc) ~init:[]
    |> Deferred.all_unit
  in
  fun () -> Throttle.enqueue sequence f

let load () =
  info "Load masters data";
  savers
  |> Stack.fold ~f:(fun acc (f,_) -> f ()::acc) ~init:[]
  |> Deferred.all_unit

let exec_in_namespace parameters =
  Rpc.Rpc.dispatch_exn
    Oci_Artefact_Api.exec_in_namespace
    (get_conf ()).conn_monitor
    parameters

exception RunnerClosed

let add_artefact_api init =
  let implement_when_open rpc f =
    let f state x =
      if state.closed then raise RunnerClosed
      else f state x
    in
    Rpc.Rpc.implement rpc f
  in
  List.fold_left ~f:Rpc.Implementations.add_exn ~init [
    (** create *)
    implement_when_open
      Oci_Artefact_Api.rpc_create
      (fun {rootfs} {Oci_Artefact_Api.src;prune;rooted_at;only_new} ->
         assert (not (Oci_Filename.is_relative src));
         let reparent = Oci_Filename.reparent ~oldd:"/" ~newd:rootfs in
         create
           ~prune:(
             Oci_Filename.make_absolute rootfs "oci"::
             Oci_Filename.make_absolute rootfs "dev"::
             Oci_Filename.make_absolute rootfs "proc"::
             Oci_Filename.make_absolute rootfs "sys"::
             Oci_Filename.make_absolute rootfs "run"::
             Oci_Filename.make_absolute rootfs "/etc/resolv.conf"::
             (List.map ~f:reparent prune))
           ~rooted_at:(reparent rooted_at)
           ~src:(reparent src)
           ~only_new
      );
    (** link_to *)
    implement_when_open
      Oci_Artefact_Api.rpc_link_to
      (fun {rootfs} (user,artefact,dst) ->
         assert (not (Oci_Filename.is_relative dst));
         let dst = Oci_Filename.make_relative "/" dst in
         let dst = Oci_Filename.make_absolute rootfs dst in
         link_to_gen (Oci_Common.master_user user) artefact dst
      );
    (** copy_to *)
    implement_when_open
      Oci_Artefact_Api.rpc_copy_to
      (fun {rootfs} (user,artefact,dst) ->
         assert (not (Oci_Filename.is_relative dst));
         let dst = Oci_Filename.make_relative "/" dst in
         let dst = Oci_Filename.make_absolute rootfs dst in
         copy_to_gen (Oci_Common.master_user user) artefact dst
      );
    (** get_internet *)
    implement_when_open
      Oci_Artefact_Api.rpc_get_internet
      (fun {rootfs} () ->
         let resolv = "etc/resolv.conf" in
         let src = Oci_Filename.make_absolute "/" resolv in
         let dst = Oci_Filename.make_absolute rootfs resolv in
         (** some distribution symlink /etc/resolv.conf to
             /run/resolve/resolv.conf *)
         Oci_Std.unlink_no_fail dst
         >>= fun () ->
         Async_shell.run "cp" ["--";src;dst]
      );
    (** git_clone *)
    implement_when_open
      Oci_Artefact_Api.rpc_git_clone
      (fun {rootfs}
        ({url;dst;user;commit}:Oci_Artefact_Api.rpc_git_clone_query) ->
        let dst = Oci_Filename.make_relative "/" dst in
        let dst = Oci_Filename.make_absolute rootfs dst in
        Oci_Git.clone ~user ~url ~dst ~commit
      );
    (** git_show_file *)
    implement_when_open
      Oci_Artefact_Api.rpc_git_copy_file
      (fun {rootfs}
        ({url;src;dst;user;commit}:Oci_Artefact_Api.rpc_git_copy_file_query) ->
        let dst = Oci_Filename.make_relative "/" dst in
        let dst = Oci_Filename.make_absolute rootfs dst in
        Oci_Git.copy_file ~user ~url ~src ~dst ~commit
      );
    (** get_file *)
    implement_when_open
      Oci_Artefact_Api.rpc_get_file
      (fun {rootfs}
        ({dst;kind;checksum}:Oci_Artefact_Api.rpc_get_file) ->
        let dst = Oci_Filename.make_relative "/" dst in
        let dst = Oci_Filename.make_absolute rootfs dst in
        Oci_Wget.get_file ~dst ~kind ~checksum
      );
    (** give_external_access *)
    implement_when_open
      Oci_Artefact_Api.rpc_give_external_access
      (fun {rootfs} src ->
        let src = Oci_Filename.make_relative "/" src in
        let src = Oci_Filename.make_absolute rootfs src in
        let conf = get_conf () in
        conf.last_external_access_id <- conf.last_external_access_id + 1;
        let id = conf.last_external_access_id in
        let dir = Oci_Filename.make_absolute conf.external_access
            (Int.to_string id) in
        let dst = Oci_Filename.make_absolute dir (Oci_Filename.basename src) in
        Unix.mkdir ~p:() ?perm:None dir
        >>= fun () ->
        Unix.symlink ~src ~dst
        >>= fun () ->
        return dst
      );
    (** get_or_release *)
    implement_when_open
      Oci_Artefact_Api.rpc_get_or_release_proc
      (fun d requested ->
         let update_cpuset got =
           begin if got = 0
             then Deferred.unit
             else update_cpuset d
           end
           >>= fun () ->
           return got
         in
         if 0 = requested then return 0
         else if 0 < requested then begin
           get_proc d.used_procs requested
           >>= fun (used_procs,l) ->
           d.used_procs <- used_procs;
           let got = List.length l in
           update_cpuset got
         end
         else begin
           let used_procs,l = release_proc d.used_procs (-requested) in
           d.used_procs <- used_procs;
           let got = List.length l in
           update_cpuset (-got)
         end
      );
  ]

let start_runner ~binary_name =
  let conf = get_conf () in
  conf.last_runner_id <- conf.last_runner_id + 1;
  let runner_id = conf.last_runner_id in
  create_used_procs ()
  >>= fun (used_procs,initial_cpuset) ->
  let rootfs = Oci_Filename.concat (get_conf ()).runners
      (Oci_Filename.mk (string_of_int runner_id)) in
  let cgroup = sprintf "runner%i" runner_id in
  let initial_state = {rootfs;used_procs;cgroup;closed=false} in
  Unix.mkdir ~p:() (Oci_Filename.concat rootfs "oci")
  >>= fun () ->
  let etc = (Oci_Filename.concat rootfs "etc") in
  Unix.mkdir ~p:() etc
  >>= fun () ->
  Async_shell.run "cp" ["/etc/resolv.conf";"-t";etc]
  >>= fun () ->
  Async_shell.run "chown" [User.pp_chown (master_user Root);"-R";"--";rootfs]
  >>= fun () ->
  let binary =
    Oci_Filename.concat (get_conf ()).binaries
      (Oci_Filename.add_extension binary_name "native") in
  let named_pipe = Oci_Filename.concat "oci" "oci_runner" in
  let parameters : Oci_Wrapper_Api.parameters = {
    rootfs = rootfs;
    idmaps =
      Oci_Wrapper_Api.idmaps
        ~first_user_mapped:conf.conf_monitor.first_user_mapped
        ~in_user:runner_user
        [Root,1000;User,1];
    command = binary;
    argv = [named_pipe];
    env =
      ["PATH","/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"];
    runuid = 0;
    rungid = 0;
    bind_system_mount = true;
    prepare_network = false;
    workdir = None;
    cgroup = Some cgroup;
    initial_cpuset = Some initial_cpuset;
  } in
  info "Start runner %s" binary_name;
  let r =
    Oci_Artefact_Api.start_in_namespace
      ~exec_in_namespace ~parameters
      ~implementations:conf.api_for_runner
      ~initial_state
      ~named_pipe:(Oci_Filename.concat rootfs named_pipe) () in
  begin
    r
    >>> fun (result,_) ->
    result
    >>> fun _ -> begin
      initial_state.closed <- true;
      initial_state.used_procs <- release_all_proc initial_state.used_procs;
      if conf.conf_monitor.cleanup_rootfs
      then Async_shell.run "rm" ["-rf";"--";rootfs]
      else return ()
    end >>> fun () ->
    ()
  end;
  r

let conn_monitor () =
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:[
        Rpc.Rpc.implement Oci_Artefact_Api.rpc_stop_runner
          (fun () () -> Oci_Artefact_Api.oci_shutdown ())
      ]
      ~on_unknown_rpc:`Raise in
  let named_pipe = Sys.argv.(1) in
  Reader.open_file (named_pipe^".in")
  >>= fun reader ->
  Writer.open_file (named_pipe^".out")
  >>= fun writer ->
  Rpc.Connection.create
    ~implementations
    ~connection_state:(fun _ -> ())
    reader writer
  >>= fun conn ->
  let conn = Result.ok_exn conn in
  return conn

let run () =
  info "Run Artefact";
  begin
    conn_monitor ()
    >>> fun conn_monitor ->
    Rpc.Rpc.dispatch_exn Oci_Artefact_Api.get_configuration conn_monitor ()
    >>> fun conf_monitor ->
    let conf = {
      last_artefact_id = -1;
      last_runner_id = -1;
      conn_monitor;
      runners = Oci_Filename.concat conf_monitor.oci_data "runners";
      binaries = Oci_Filename.concat conf_monitor.oci_data "binaries";
      storage = Oci_Filename.concat conf_monitor.oci_data "storage";
      permanent = Oci_Filename.concat conf_monitor.oci_data "permanent";
      log = Oci_Filename.concat conf_monitor.oci_data "log";
      git = Oci_Filename.concat conf_monitor.oci_data "git";
      wget = Oci_Filename.concat conf_monitor.oci_data "wget";
      external_access =
        Oci_Filename.concat conf_monitor.oci_data "external_access";
      last_external_access_id = -1;
      conf_monitor;
      api_for_runner = add_artefact_api !masters;
      proc = create_proc conf_monitor.proc;
    } in
    Ivar.fill gconf_ivar conf;
    if conf_monitor.debug_level then Log.Global.set_level `Debug;
    Async_shell.run "rm" ["-rf";"--";
                          conf.runners;conf.binaries;conf.external_access;]
    >>> fun () ->
    Deferred.List.iter ~f:(Unix.mkdir ~p:() ?perm:None)
      [conf.runners; conf.binaries; conf.external_access;
       conf.storage; conf.permanent;
       conf.log; conf.git; conf.wget]
    >>> fun () ->
    (** Copy binaries *)
    Sys.ls_dir conf_monitor.binaries
    >>> fun files ->
    let files = List.filter_map
        ~f:(fun f -> if String.is_suffix f ~suffix:"native"
             then Some f else None)
        files in
    begin if files = [] then return ()
    else
      Async_shell.run "cp" (["-t";conf.binaries;"--"]@
                            List.map
                              ~f:(Oci_Filename.concat conf_monitor.binaries)
                              files)
    end
    >>> fun () ->
    (** Write ssh key *)
    Deferred.Option.bind
      (return conf.conf_monitor.identity_file)
      (fun src ->
         let dst = Oci_Filename.concat conf.binaries "ssh_identity" in
         Oci_Std.unlink_no_fail dst
         >>= fun () ->
         Writer.open_file ~perm:0o400 dst
         >>= fun writer ->
         Writer.write writer src;
         Writer.close writer
         >>= fun () ->
         Deferred.Option.return dst
      )
    >>> fun identity_file ->
    Deferred.all_unit
      (List.map
         ~f:(fun x -> Unix.chmod ~perm:0o555
                (Oci_Filename.concat conf.binaries x))
         files)
    >>> fun () ->
    register_saver ~name:"artifact"
      ~loader:loader_artifact_data ~saver:saver_artifact_data;
    Oci_Git.init ~dir:conf.git ~register_saver:(register_saver ~name:"Git")
      ~identity_file;
    Oci_Wget.init ~dir:conf.wget;
    load ()
    >>> fun () ->
    let save_at = Time.Span.create ~min:10 () in
    Clock.every' ~start:(after save_at) save_at save;
    Oci_Artefact_Api.oci_at_shutdown save;
    let socket = Oci_Filename.concat conf_monitor.oci_data "oci.socket" in
    Async_shell.run "rm" ["-f";"--";socket]
    >>> fun () ->
    Rpc.Connection.serve
      ~where_to_listen:(Tcp.on_file socket)
      ~initial_connection_state:(fun _ _ -> {rootfs="external socket";
                                             used_procs=dumb_used_procs;
                                             cgroup="external socket";
                                             (** not an open runner *)
                                             closed=true;
                                            })
      ~implementations:!masters
      ()
    >>> fun server ->
    Shutdown.at_shutdown (fun () ->
        Deferred.ignore (Clock.with_timeout
                           (Time.Span.create ~sec:10 ())
                           (Tcp.Server.close server)));
    Unix.chmod socket ~perm:0o777
    >>> fun () ->
    ()
  end;
  Scheduler.go ()
