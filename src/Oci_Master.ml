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

let oci_at_shutdown = Oci_Artefact_Api.oci_at_shutdown

type runner= Oci_Artefact.runner

let register = Oci_Artefact.register_master
let register_saver = Oci_Artefact.register_saver

let run () = Oci_Artefact.run ()
let start_runner ~debug_info ~binary_name ?slot () =
  Oci_Artefact.start_runner ~debug_info ~binary_name ?slot ()
let stop_runner runner = Oci_Artefact.stop_runner runner
type slot = Oci_Artefact.slot
let alloc_slot = Oci_Artefact.alloc_slot
let freeze_runner = Oci_Artefact.freeze_runner
let unfreeze_runner = Oci_Artefact.unfreeze_runner
let permanent_directory = Oci_Artefact.permanent_directory

type exists_log =
  | Log: 'a Oci_Log.writer -> exists_log

let log_id: exists_log Type_equal.Id.t =
  Type_equal.Id.create ~name:"Oci_Master.log.t" sexp_of_opaque

let get_log () =
  Option.value_exn ~message:"No log currently attached"
    (Scheduler.find_local log_id)

let attach_log l f =
  Scheduler.with_local log_id (Some (Log l)) ~f

let simple_register_saver ?(init=(fun () -> return ())) ~basename
    ~loader ~saver data bin_t =
  Oci_Artefact.register_saver
    ~name:(Oci_Data.name data)
    ~loader:(fun () ->
      permanent_directory data
      >>= fun dir ->
      init ()
      >>= fun () ->
      let file = Oci_Filename.make_absolute dir basename in
      Oci_Std.read_if_exists file bin_t.Bin_prot.Type_class.reader loader
    )
    ~saver:(fun () ->
      saver ()
      >>= fun r ->
      permanent_directory data
      >>= fun dir ->
      let file = Oci_Filename.make_absolute dir basename in
      Oci_Std.backup_and_open_file file
      >>= fun writer ->
      Writer.write_bin_prot writer bin_t.Bin_prot.Type_class.writer r;
      Writer.close writer
      )

let simple_runner ~debug_info ~binary_name
    ?error f =
  start_runner ~debug_info ~binary_name ()
  >>= fun (err,runner) ->
  choose [
    choice (err >>= function
      | Ok () -> never ()
      | Error s -> return s)
      (match error with
       | None -> (fun e -> invalid_argf "error %s" (Error.to_string_hum e) ())
       | Some error -> error
      );
    choice begin
      Monitor.protect ~here:[%here]
        ~finally:(fun () -> stop_runner runner)
        ~name:"simple_runner"
        (fun () -> f runner)
    end (fun x -> x);
  ]


type reusable = {
  runner: runner;
  wait: unit Or_error.t Deferred.t;
}

let reusable_id r = Oci_Artefact.runner_id r.runner

let reusable_runner
    ~(hashable_key:'k Hashtbl.Hashable.t)
    ~debug_info
    ~binary_name
    ?(timeout=Time.Span.create ~sec:10 ())
    ?error
    (f: first:bool -> runner -> 'k -> 'd -> 'a Or_error.t Deferred.t) =
  let h = Hashtbl.create ~hashable:hashable_key () in
  let scheduled (_,event,_) =
    match Clock.Event.status event with
    | `Aborted _ ->
      Log.Global.error "reusable runner aborted still in reusable list";
      false
    | `Scheduled_at _ -> true
    | `Happened _ -> false in
  Clock.run_at_intervals
    ~continue_on_error:true
    (Time.Span.create ~min:1 ())
    (fun () -> Hashtbl.filter_map_inplace h
        ~f:(fun l -> let l = List.filter ~f:scheduled l in
             if List.is_empty l then None else Some l));
  (* partial application *)
  fun k d ->
    let debug_info = debug_info k in
    alloc_slot ()
    >>= fun slot ->
    let rec find_available () =
      match Hashtbl.find h k with
      | None | Some [] ->
        start_runner
          ~debug_info
          ~binary_name:(binary_name k)
          ~slot ()
        >>= fun (err,runner) ->
        let reusable = {runner;wait=err} in
        Log.Global.debug "Reusable runner %i started for %s"
          (reusable_id reusable)
          debug_info;
        return (reusable,true)
      | Some ((reusable,event,freezing)::l) ->
        if l = []
        then Hashtbl.remove h k
        else Hashtbl.set h ~key:k ~data:l;
        match Clock.Event.abort event () with
        | `Previously_happened _ -> find_available ()
        | `Previously_aborted _ ->
          Log.Global.error "Reusable runner %i aborted still in reusable list"
            (reusable_id reusable);
          find_available ()
        | `Ok ->
          freezing >>= fun () ->
          match Deferred.peek reusable.wait with
          | None ->
            Log.Global.info "Reuse runner %i for %s"
              (reusable_id reusable)
              debug_info;
            unfreeze_runner reusable.runner slot
            >>= fun () ->
            return (reusable,false)
          | Some e ->
            Log.Global.error "Reusable runner %i stopped when idle: %s"
              (reusable_id reusable)
              (Sexp.to_string_hum ([%sexp_of: (unit,Error.t) Result.t] e));
            find_available ()
    in
    find_available ()
    >>= fun (reusable,first) ->
    choose [
      choice (reusable.wait >>= function
        | Ok () -> never ()
        | Error s -> return s)
        (fun x -> `Stopped x);
      choice begin
        Monitor.try_with_join_or_error ~here:[%here]
          ~name:"reusable_runner"
          (fun () -> f ~first reusable.runner k d)
      end (fun x -> `Runned x);
    ]
    >>= function
    | `Runned (Ok _ as x) ->
      Log.Global.debug "Reusable runner %i freezed"
        (reusable_id reusable);
      let event = Clock.Event.run_after timeout (fun () ->
          Log.Global.debug "Reusable runner %i stopped since not used"
            (reusable_id reusable);
          stop_runner reusable.runner
          >>> fun () ->
          reusable.wait
          >>> function
          | Ok () -> ()
          | Error e ->
            Log.Global.error
              "A reusable runner stopped with error after timeout: %s"
              (Sexp.to_string_hum ([%sexp_of: Error.t] e));
        ) () in
      Hashtbl.add_multi h ~key:k
        ~data:(reusable,event,freeze_runner reusable.runner);
      return x
    | `Runned (Error e as x) ->
      (* we stop the runner that "failed" previously *)
      Log.Global.debug "Reusable runner %i failed: %s"
        (reusable_id reusable)
        (Sexp.to_string_hum ([%sexp_of: Error.t] e));
      begin
        stop_runner reusable.runner
        >>> fun () ->
        reusable.wait
        >>>  function
        | Ok () -> ()
        | Error e ->
          Log.Global.error
            "A reusable runner that failed stopped with error: %s"
            (Sexp.to_string_hum ([%sexp_of: Error.t] e));
      end;
      return x
    | `Stopped e  ->
      match error with
      | None -> invalid_argf "error %s" (Error.to_string_hum e) ()
      | Some error -> return (Ok (error k d e))


let simple_master f q =
  Oci_Log.init_writer (fun log ->
      Monitor.try_with_or_error ~here:[%here]
        ~name:"Oci_Master.simple_master"
        (fun () -> attach_log log (fun () -> f q))
      >>= fun res ->
      Oci_Log.write_and_close log res
    )

let simple_master_unit f q =
  Oci_Log.init_writer (fun log ->
      Monitor.try_with_or_error ~here:[%here]
        ~name:"Oci_Master.simple_master"
        (fun () -> attach_log log (fun () -> f q log))
      >>= fun res ->
      Oci_Log.close_writer log res
    )


module Make(Query : Hashtbl.Key_binable) (Result : Binable.S) = struct
  module H = Hashtbl.Make(Query)


  module CreateMaster(S:sig
      val data: (Query.t,Result.t) Oci_Data.t
    end) = struct

    module Log = Oci_Log.Make(struct
        let dir =
          permanent_directory S.data
          >>= fun d ->
          return (Oci_Filename.make_absolute d "log")
        let register_saver =
          register_saver ~name:((Oci_Data.name S.data)^"@log")
        include Result
      end)

    let create_master_unit f =
      let db : Log.t H.t =
        H.create () in
      let f q =
        match H.find db q with
        | Some r -> Log.reader r
        | None ->
          let log = Log.create () in
          H.add_exn db ~key:q ~data:log;
          don't_wait_for begin
            Monitor.try_with_or_error ~here:[%here]
              ~name:"Oci_Master.Make.create_master"
              (fun () ->
                 let log = Log.writer log in
                 attach_log log (fun () -> f q log))
            >>= fun e ->
            Log.add_without_pushback log (Oci_Log._end e);
            begin match e with
              | Ok _ -> ()
              | Error _ -> H.remove db q (* anomaly are not saved *)
            end;
            Log.close log
          end;
          Log.reader log
      in
      let forget q =
        if H.mem db q then H.remove db q;
        Deferred.Or_error.return ()
      in
      let name = Oci_Data.name S.data in
      register_saver
        ~name
        ~loader:(fun () ->
            permanent_directory S.data
            >>= fun dir ->
            let file = Oci_Filename.make_absolute dir "data" in
            Async.Std.Log.Global.debug "Load master %s" name;
            if not (H.is_empty db) then begin
              Async.Std.Log.Global.error
                "Master %s have already been loaded" name;
              H.clear db;
            end;
            Oci_Std.read_list_if_exists file
              [% bin_reader: (Query.t * Log.t)]
              (fun (q,l) ->
                 match H.add db ~key:q ~data:l with
                 | `Ok -> ()
                 | `Duplicate ->
                   Async.Std.Log.Global.error
                     "Duplicate key %s"
                     (Sexp.to_string_hum ~indent:1
                        (Query.sexp_of_t q))
              )
            >>= fun () ->
            debug "Master %s load %i records" name (H.length db);
            Deferred.unit
          )
        ~saver:(fun () ->
            let l = H.fold ~init:[]
                ~f:(fun ~key ~data:log acc ->
                    if Log.is_closed log
                    then (key,log)::acc
                    else acc
                  ) db in
            debug "Master %s save %i records" name (List.length l);
            permanent_directory S.data
            >>= fun dir ->
            let file = Oci_Filename.make_absolute dir "data" in
            Oci_Std.backup_and_save_list file
              [% bin_writer: (Query.t * Log.t)]
              (fun f -> List.iter ~f l)
          );
      register ~forget S.data f

  end

  let create_master_unit data f =
    let module M = CreateMaster(struct let data = data end) in
    M.create_master_unit f

  let create_master data f =
    create_master_unit data
      (fun q log ->
         f q
         >>= fun r ->
         Pipe.write_without_pushback log (Oci_Log.data r);
         return ())

  let create_master_and_runner data ?(binary_name=Oci_Data.name data) ?error f =
    create_master data (fun q -> simple_runner
                         ~debug_info:(Oci_Data.name data)
                         ~binary_name ?error
                         (fun runner -> f runner q))

  let create_master_and_reusable_runner
      data ?(binary_name=Oci_Data.name data) ?error
      ~hashable_key ~extract_key ?timeout f =
    let reusable =
      reusable_runner
        ~hashable_key
        ~debug_info:(fun _ -> Oci_Data.name data)
        ~binary_name:(fun _ -> binary_name)
        ?timeout
        ?error:(Option.map ~f:(fun error _ _ -> error) error)
        (fun ~first runner _ q ->
           f ~first runner q
           >>= fun e -> return (Ok e))
    in
    create_master data (fun q ->
        reusable (extract_key q) q
        >>= fun e -> return (Or_error.ok_exn e) )

end

let write_log kind fmt =
  Printf.ksprintf (fun s ->
      match get_log () with
      | Log log ->
        s
        |> String.split_lines
        |> List.iter ~f:(fun line ->
            Pipe.write_without_pushback log (Oci_Log.line kind line)
          )
    ) fmt

let std_log fmt = write_log Oci_Log.Standard fmt
let err_log fmt = write_log Oci_Log.Error fmt
let cmd_log fmt = write_log Oci_Log.Command fmt
let cha_log fmt = write_log Oci_Log.Chapter fmt


exception NoResult

let print_msg msg =
  msg
  |> Option.map ~f:(fun s -> " "^s)
  |> Option.value ~default:""

let dispatch_runner ?msg d r q =
  Oci_Artefact.runner_conn_or_never r
  >>= fun t ->
  match get_log () with
  | Log log ->
    cmd_log "dispatch runner %s%s" (Oci_Data.name d) (print_msg msg);
    let r = Ivar.create () in
    begin
      Rpc.Pipe_rpc.dispatch (Oci_Data.log d) t q
      >>= fun res -> match Or_error.join res with
      | Error _ as err -> Ivar.fill r err; Deferred.unit
      | Ok (p,_) ->
        let p = Pipe.map p ~f:(function
            | {Oci_Log.data=(Std _ | End (Ok ()));_} as l -> l
            | {Oci_Log.data=Oci_Log.Extra res} ->
              Ivar.fill r (Ok res);
              Oci_Log.line Oci_Log.Standard "result received"
            | {Oci_Log.data=Oci_Log.End (Error res)} ->
              Ivar.fill r (Error res);
              Oci_Log.line Oci_Log.Error "error received"
          )
        in
        upon (Pipe.closed p)
          (fun () -> Ivar.fill_if_empty r (Or_error.of_exn NoResult));
        Pipe.transfer_id p log
    end
    >>= fun () ->
    Ivar.read r

let dispatch_runner_exn ?msg d r q =
  Oci_Artefact.runner_conn_or_never r
  >>= fun t ->
  match get_log () with
  | Log log ->
    cmd_log "dispatch runner %s%s" (Oci_Data.name d) (print_msg msg);
    let r = Ivar.create () in
    Rpc.Pipe_rpc.dispatch_exn (Oci_Data.log d) t q
    >>= fun (p,_) ->
    let p = Pipe.map p ~f:(function
        | {Oci_Log.data=(Std _ | End (Ok ()));_} as l -> l
        | {Oci_Log.data=Oci_Log.Extra res} ->
          Ivar.fill_if_empty r res;
          Oci_Log.line Oci_Log.Standard "result received"
        | {Oci_Log.data=Oci_Log.End (Result.Error err)} ->
          Pipe.write_without_pushback log
            (Oci_Log.line Oci_Log.Error "error received");
          Error.raise err
      )
    in
    upon (Pipe.closed p)
      (fun () -> if Ivar.is_empty r then raise NoResult);
    Pipe.transfer_id p log
    >>= fun () ->
    Ivar.read r

let dispatch_runner_log ?msg log d r q =
  Oci_Artefact.runner_conn_or_never r
  >>= fun t ->
  cmd_log "dispatch runner %s%s" (Oci_Data.name d) (print_msg msg);
  Rpc.Pipe_rpc.dispatch_exn (Oci_Data.log d) t q
  >>= fun (p,_) ->
  Pipe.transfer_id p log


let dispatch_master ?msg d q =
  cmd_log "dispatch master %s%s" (Oci_Data.name d) (print_msg msg);
  Oci_Artefact.dispatch_master d q

let dispatch_master_log ?msg d q =
  cmd_log "dispatch master %s%s" (Oci_Data.name d) (print_msg msg);
  Oci_Artefact.dispatch_master_log d q

let dispatch_master_exn ?msg d q =
  cmd_log "dispatch master %s%s" (Oci_Data.name d) (print_msg msg);
  Oci_Artefact.dispatch_master_exn d q

let oci_version = Oci_Version.version
