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

type kind =
  | Standard | Error | Chapter | Command
    with sexp, bin_io

type line = {
  kind : kind;
  line : string;
  time : Time.t;
} with sexp, bin_io

(* let line_invariant line = not (String.contains line.line '\n') *)

let line kind line =
  {kind;line;time=Time.now ()}

let color_of_kind = function
  | Standard -> `Black
  | Error -> `Red
  | Chapter -> `Underscore
  | Command -> `Blue

module Log_Id : Int_intf.S = Int

include Log_Id

let permanent_dir = ref ""
let next_id = ref (of_int_exn 0)
let null : t = (of_int_exn 0)

(** The database of log being currently, the one that already ended
    (in this session or a previous one) are stored on disk
*)
let db_log: line Oci_Queue.t Table.t = Table.create ()

let log_file id =
  Oci_Filename.make_absolute !permanent_dir (to_string id)


let read_from_file id =
  Reader.open_file (log_file id)
  >>= fun reader ->
  let pipe,_ = Unpack_sequence.unpack_into_pipe
      ~from:(Unpack_sequence.Unpack_from.Reader reader)
      ~using:(Unpack_buffer.create_bin_prot bin_reader_line) in
  return pipe

let read id =
  match Table.find db_log id with
  | None -> read_from_file id
  | Some q -> return (Oci_Queue.read q)

let create () =
  incr next_id;
  let id = !next_id in
  (** create the queue storage *)
  let q = Oci_Queue.create () in
  Table.add_exn db_log ~key:id ~data:q;
  (** write to disk *)
  don't_wait_for begin
    Writer.open_file (log_file id)
    >>= fun writer ->
    (** When the log end, new reader will read from the file *)
    Writer.transfer writer
      (Oci_Queue.read q)
      (Writer.write_bin_prot writer bin_writer_line)
    >>= fun () ->
    Writer.close writer
    >>= fun () ->
    Table.remove db_log id;
    return ()
  end;
  id

exception Closed_Log

let transfer id p =
  match Table.find db_log id with
  | None -> raise Closed_Log
  | Some q -> Oci_Queue.transfer_id q p

let write_without_pushback id line =
  match Table.find db_log id with
  | None -> raise Closed_Log
  | Some q -> Oci_Queue.add_without_pushback q line

let close id =
  match Table.find db_log id with
  | None -> return ()
  | Some q -> Oci_Queue.close q

let init ~dir ~register_saver =
  permanent_dir := dir;
  let data = Oci_Filename.make_absolute dir "data" in
  register_saver
    ~loader:(fun () ->
        Oci_Std.read_if_exists data bin_reader_t
          (fun x -> next_id := x; return ())
        >>= fun () ->
        (** create null file *)
        Writer.open_file (log_file null)
        >>= fun writer ->
        Writer.close writer
      )
    ~saver:(fun () ->
        Oci_Std.backup_and_open_file data
        >>= fun writer ->
        Writer.write_bin_prot writer bin_writer_t !next_id;
        Writer.close writer
      )

let t_type_id = Type_equal.Id.create ~name:"Oci_Log.t" sexp_of_t
