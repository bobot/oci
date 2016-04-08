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
