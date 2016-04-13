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

let version = 2

type kind =
  | Standard | Error | Chapter | Command
[@@deriving sexp, bin_io]

type 'a data =
  | Std of kind * string
  | Extra of 'a
  | End of unit Or_error.t
[@@deriving sexp, bin_io]

type 'a line = {
  data : 'a data;
  time : Time.t;
} [@@deriving sexp, bin_io]

(* let line_invariant line = not (String.contains line.line '\n') *)

let line kind line =
  {data=Std(kind,line);time=Time.now ()}
let data data =
  {data=Extra data;time=Time.now ()}
let _end e =
  {data=End e;time=Time.now ()}

let map_line f = function
  | { data = Extra x; time } -> {data = Extra(f x); time }
  | { data = (Std _ | End _) } as x -> x

let color_of_kind = function
  | Standard -> `Black
  | Error -> `Red
  | Chapter -> `Underscore
  | Command -> `Blue

type 'result writer = 'result line Pipe.Writer.t
let close_writer w e =
  Pipe.write w (_end e)
  >>= fun () ->
  Pipe.close w;
  Deferred.unit

let write_and_close w e =
  begin match (e:'r Or_error.t) with
  | Ok r ->
    Pipe.write w (data r)
    >>= fun () ->
    Pipe.write w (_end (Ok ()))
  | Error e ->
    Pipe.write w (_end (Error e))
  end
  >>= fun () ->
  Pipe.close w;
  Deferred.unit

type 'result reader = {mutable state: (unit -> 'result line Pipe.Reader.t)}

let reader t = {state = fun () -> Oci_Queue.reader t}
let read t = t.state ()
let create () = Oci_Queue.create ()
let close = Oci_Queue.close
let init f =
  let log = create () in
  don't_wait_for (f log >>= fun () -> close log);
  reader log
let init_writer f =
  init (fun log -> f (Oci_Queue.writer log))

let reader_stop_after ~f t = {
  state = fun () ->
    let reached = ref false in
    Pipe.filter (t.state ()) ~f:(function
        | _ when !reached -> false
        | { data = (Std _ | End _) } -> true
        | { data= Extra e } ->
          if f e then reached := true;
          true
      )
}

let reader_get_first ~f t =
    let rec aux r =
      Pipe.read' r
      >>= function
      | `Eof -> return `Incomplete
      | `Ok q ->
        match
          Queue.find_map q ~f:(function
              | { data = Extra e } when f e -> Some (`Found e)
              | { data = End (Ok ()) } -> Some `NotFound
              | { data = End (Error err) } -> Some (`Error err)
              | _ -> None)
        with
        | None -> aux r
        | Some x -> return x
    in
    aux (t.state ())

exception Closed_Log

module Make(S: sig
    val dir: Oci_Filename.t Deferred.t
    val register_saver:
      loader:(unit -> unit Deferred.t) ->
      saver:(unit -> unit Deferred.t) ->
      unit
    type t [@@deriving bin_io]
  end) = struct
  module Log_Id : Int_intf.S = Int

  include Log_Id

  let next_id = ref (of_int_exn 0)
  let null : t = (of_int_exn 0)

  (** The database of log being currently, the one that already ended
      (in this session or a previous one) are stored on disk
  *)
  let db_log: (S.t line Oci_Queue.t * S.t reader) Table.t = Table.create ()

  let dir = S.dir
    >>= fun dir ->
    let dir = Oci_Filename.make_absolute dir (sprintf "v%i" version) in
    Unix.mkdir ~p:() dir
    >>= fun () -> return dir

  let log_file id =
    dir
    >>= fun d ->
    return (Oci_Filename.make_absolute d (to_string id))

  let read_from_file id =
    Pipe.init (fun w ->
        log_file id
        >>= fun file ->
        Reader.open_file file
        >>= fun reader ->
        let pipe,_ = Unpack_sequence.unpack_into_pipe
            ~from:(Unpack_sequence.Unpack_from.Reader reader)
            ~using:(Unpack_buffer.create_bin_prot
                      (bin_reader_line S.bin_reader_t)) in
        Pipe.transfer_id pipe w
        >>= fun () ->
        Reader.close reader
      )

  let read id =
     match Table.find db_log id with
    | None -> read_from_file id
    | Some (q,_) -> Oci_Queue.reader q

  let reader id =
    match Table.find db_log id with
    | None -> { state = fun () -> read_from_file id }
    | Some (_,r) -> r

  let writer id =
    match Table.find db_log id with
    | None -> raise Closed_Log
    | Some (q,_) -> Oci_Queue.writer q

  let create () =
    incr next_id;
    let id = !next_id in
    (* create the queue storage *)
    let q = Oci_Queue.create () in
    let r = { state = fun () -> Oci_Queue.reader q } in
    Table.add_exn db_log ~key:id ~data:(q,r);
    (* write to disk *)
    don't_wait_for begin
      log_file id
      >>= fun log_file ->
      let log_file_part = Oci_Filename.add_extension log_file "part" in
      Writer.open_file log_file_part
      >>= fun writer ->
      (* When the log end, new readers will read from the file *)
      Writer.transfer writer
        (Oci_Queue.reader q)
        (Writer.write_bin_prot writer
           (bin_writer_line S.bin_writer_t))
      >>= fun () ->
      Writer.close writer
      >>= fun () ->
      Unix.rename ~src:log_file_part ~dst:log_file
      >>= fun () ->
      r.state <- (fun () -> read_from_file id);
      Table.remove db_log id;
      return ()
    end;
    id

  let transfer id p =
    match Table.find db_log id with
    | None -> raise Closed_Log
    | Some (q,_) -> Oci_Queue.transfer_id q p

  let add_without_pushback id line =
    match Table.find db_log id with
    | None -> raise Closed_Log
    | Some (q,_) -> Oci_Queue.add_without_pushback q line

  let close id =
    match Table.find db_log id with
    | None -> return ()
    | Some (q,_) -> Oci_Queue.close q

  let is_closed id = not (Table.mem db_log id)

  let () =
    let data =
      dir
      >>= fun dir ->
      return (Oci_Filename.make_absolute dir "data") in
    S.register_saver
      ~loader:(fun () ->
          data >>= fun data ->
          Oci_Std.read_if_exists data bin_reader_t
            (fun x -> next_id := x; return ())
          >>= fun () ->
          (* create null file *)
          log_file null
          >>= fun log_file ->
          Writer.open_file log_file
          >>= fun writer ->
          Writer.close writer
        )
      ~saver:(fun () ->
          data >>= fun data ->
          Oci_Std.backup_and_open_file data
          >>= fun writer ->
          Writer.write_bin_prot writer bin_writer_t !next_id;
          Writer.close writer
        )

end
