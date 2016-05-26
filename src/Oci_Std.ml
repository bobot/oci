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


module Pp = Oci_pp
include Log.Global

let unlink_no_fail filename =
  (* Sys.file_exists follows symlink *)
  Monitor.try_with ~here:[%here]
    (fun () -> Unix.lstat filename)
  >>= function
  | Ok _ -> Unix.unlink filename
  | Error _ -> return ()
  (* | Error (Unix.Unix_error _) -> return () *)
  (* | Error exn -> raise exn *)

let unlink_no_fail_blocking filename =
  let open Core.Std in
  (* Sys.file_exists follows symlink *)
  try
    ignore (Unix.lstat filename);
    Unix.unlink filename
  with _ -> ()

let backup_and_open_file file =
  let file_bak = Oci_Filename.add_extension file "bak" in
  Sys.file_exists_exn file
  >>= fun exi ->
  begin if exi then begin
      unlink_no_fail file_bak
      >>= fun () ->
      Unix.rename ~src:file ~dst:file_bak
    end
    else return ()
  end
  >>= fun () ->
  Writer.open_file file

let open_if_exists file f =
  Sys.file_exists_exn file
  >>= fun exi ->
  if exi then begin
    Reader.open_file file
    >>= fun reader ->
    f reader
    >>= fun () ->
    Reader.close reader
  end
  else return ()

let read_if_exists file bin_reader_t f =
  open_if_exists file
    (fun reader ->
       Reader.read_bin_prot reader bin_reader_t
       >>= function
       | `Eof -> return ()
       | `Ok r -> f r
    )

external wait4: Caml.Unix.wait_flag list -> int ->
  int * Caml.Unix.process_status * Core.Core_unix.Resource_usage.t = "oci_wait4"
let wait4 pid =
  let pid = (Pid.to_int pid) in
  In_thread.syscall_exn ~name:"wait4"
    (fun () -> wait4 [] pid)
  >>= fun (pid',status,ru) ->
  assert (pid' = pid);
  return (Core.Core_unix.Exit_or_signal.of_unix status, ru)

module Oci_Unix : sig
  type t [@@deriving sexp_of]

  (** accessors *)
  val pid    : t -> Pid.t
  val stdin  : t -> Writer.t
  val stdout : t -> Reader.t
  val stderr : t -> Reader.t

  type 'a create
    =  ?env         : Core.Std.Unix.env  (** default is [`Extend []] *)
    -> ?working_dir : string
    -> prog         : string
    -> args         : string list
    -> unit
    -> 'a Deferred.t
  val create     : t Or_error.t create
  val create_exn : t            create

  val start : t -> unit Deferred.t
  val wait : t -> Unix.Exit_or_signal.t Deferred.t
end
= struct
  (** from core core_unix *)


let atom x = Sexp.Atom x
let list x = Sexp.List x

let record l =
  list (List.map l ~f:(fun (name, value) -> list [atom name; value]))
;;

(* No need to include a counter here. It just doesn't make sense to think we are
going to be receiving a steady stream of interrupts.
   Glibc's macro doesn't have a counter either.
*)
let rec retry_until_no_eintr f =
  try
    f ()
  with Unix.Unix_error (EINTR, _, _) ->
    retry_until_no_eintr f


(* This wrapper improves the content of the Unix_error exception
   raised by the standard library (by including a sexp of the function
   arguments), and it optionally restarts syscalls on EINTR. *)
let improve ?(restart = false) f make_arg_sexps =
  try
    if restart then retry_until_no_eintr f else f ()
  with
  | Unix.Unix_error (e, s, _) ->
    let buf = Buffer.create 100 in
    let fmt = Format.formatter_of_buffer buf in
    Format.pp_set_margin fmt 10000;
    Sexp.pp_hum fmt (record (make_arg_sexps ()));
    Format.pp_print_flush fmt ();
    let arg_str = Buffer.contents buf in
    raise (Unix.Unix_error (e, s, arg_str))
;;

let env_assignments env =
  match env with
  | `Replace_raw env -> env
  | (`Replace _ | `Extend _) as env ->
    let env_map =
      let current, env =
        match env with
        | `Replace env -> [], env
        | `Extend env ->
          let current =
            List.map (Array.to_list (Unix.environment ()))
              ~f:(fun s -> String.lsplit2_exn s ~on:'=')
          in
          current, env
      in
      List.fold_left (current @ env) ~init:String.Map.empty
        ~f:(fun map (key, data) -> Map.add map ~key ~data)
    in
    Map.fold env_map ~init:[]
      ~f:(fun ~key ~data acc -> (key ^ "=" ^ data) :: acc)

module Process_info = struct
  (* Any change to the order of these fields must be accompanied by a
     corresponding change to oci_fork_exec.c:oci_ml_create_process. *)
  type t =
    { pid : Pid.t;
      stdin : Core.Std.Unix.File_descr.t;
      stdout : Core.Std.Unix.File_descr.t;
      stderr : Core.Std.Unix.File_descr.t;
      start  : Core.Std.Unix.File_descr.t;
    }
  [@@deriving sexp]
end

external create_process
  :  ?working_dir : string
  -> prog : string
  -> args : string array
  -> env : string array
  -> search_path : bool
  -> Process_info.t
  = "oci_ml_create_process"

let create_process_env ?working_dir ~prog ~args ~env () =
  create_process
    ?working_dir
    ~search_path:true
    ~prog
    ~args:(Array.of_list args)
    ~env:(Array.of_list (env_assignments env))

let create_process_env ?working_dir ~prog ~args ~env () =
  improve (fun () -> create_process_env ?working_dir ~prog ~args ~env ())
    (fun () ->
      [("prog", atom prog);
       ("args", sexp_of_list atom args);
       ("env", Core.Std.Unix.sexp_of_env env)])

type t =
  { pid         : Pid.t
  ; stdin       : Writer.t
  ; stdout      : Reader.t
  ; stderr      : Reader.t
  ; start       : Writer.t
  ; prog        : string
  ; args        : string list
  ; working_dir : string option
  ; env         : Core.Std.Unix.env
  }
[@@deriving fields, sexp_of]

let wait t = Unix.waitpid t.pid
let start t = Writer.close t.start

type 'a create
  =  ?env         : Core.Std.Unix.env
  -> ?working_dir : string
  -> prog         : string
  -> args         : string list
  -> unit
  -> 'a Deferred.t

let create ?(env = `Extend []) ?working_dir ~prog ~args () =
  In_thread.syscall ~name:"oci_create_process_env" (fun () ->
    create_process_env ~prog ~args ~env ?working_dir ())
  >>| function
  | Error exn -> Or_error.of_exn exn
  | Ok { Process_info.pid; stdin; stdout; stderr; start } ->
    let create_fd name file_descr =
      Fd.create Fifo file_descr
        (Info.create "child process" ~here:[%here] (name, `pid pid, `prog prog,
                                                    `args args)
           [%sexp_of:
                      string * [ `pid of Pid.t ] * [ `prog of string ] *
                      [ `args of string list ]
            ])
    in
    Ok { pid
       ; stdin  = Writer.create (create_fd "stdin"  stdin )
       ; stdout = Reader.create (create_fd "stdout" stdout)
       ; stderr = Reader.create (create_fd "stderr" stderr)
       ; start  = Writer.create (create_fd "start"  start )
       ; prog
       ; args
       ; working_dir
       ; env
       }

let create_exn ?env ?working_dir ~prog ~args () =
  create ?env ?working_dir ~prog ~args () >>| ok_exn

end
