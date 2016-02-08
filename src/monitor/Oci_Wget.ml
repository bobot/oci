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


let permanent_dir = ref ""
let tmp_path =
  let i = ref (-1) in
  fun () -> incr i;
    Oci_Filename.make_absolute
      (Oci_Filename.make_absolute !permanent_dir "tmp_dir")
      (Int.to_string !i)

let path_of_checksum s = function
  | `MD5 -> Oci_Filename.make_absolute
              (Oci_Filename.make_absolute !permanent_dir "md5")
              s

let db_md5sum
  : unit Sequencer.t String.Table.t = String.Table.create ()


let download_file_aux ~kind ~checksum ~url =
  let tmp = tmp_path () in
  Unix.mkdir ~p:() (Oci_Filename.dirname tmp)
  >>= fun () ->
  Oci_Std.unlink_no_fail tmp
  >>= fun () ->
  Async_shell.test
    ~true_v:[0]
    ~false_v:[1;4;8;2;3;5;6;7]
    "wget" ["-T";"30";"--quiet";"-O";tmp;url]
  >>= fun ok ->
  if not (ok)
  then invalid_argf "Can't download at %s" url ()
  else
    begin
      match kind with
      | `MD5 -> Async_shell.test
                  ~input:(sprintf "%s %s" checksum tmp)
                  "md5sum" ["--check";"--quiet";"--status";"--strict"]
    end
    >>= fun b ->
    if b
    then begin
      let store = path_of_checksum checksum kind in
      Unix.mkdir ~p:() (Oci_Filename.dirname store)
      >>= fun () ->
      Unix.rename ~src:tmp ~dst:store
    end
    else begin
      Unix.unlink tmp
      >>= fun () ->
      invalid_argf "Checksum %s failed for %s" checksum url ()
    end

let download_file ~kind ~checksum ~url =
  let store = path_of_checksum checksum kind in
  Sys.file_exists_exn store
  >>= fun b -> begin
    if b then Deferred.unit
    else
      let seq =
        match String.Table.find db_md5sum checksum with
        | Some seq -> seq
        | None ->
          let seq = Sequencer.create ~continue_on_error:true () in
          String.Table.add_exn db_md5sum ~key:url ~data:seq;
          seq
      in
      Throttle.enqueue seq (fun () ->
          Sys.file_exists_exn store
          >>= fun b -> begin
            if b then Deferred.unit
            else download_file_aux ~kind ~checksum ~url
          end)
  end

let get_file ~kind ~checksum ~dst =
  let store = path_of_checksum checksum kind in
  Sys.file_exists_exn store
  >>= fun b ->
  if not b
  then invalid_argf "File with checksum %s have not been downloaded" checksum ()
  else Unix.link ~target:store ~link_name:dst ()

let init ~dir =
  permanent_dir := dir;
