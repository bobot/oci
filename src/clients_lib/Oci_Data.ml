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
open Rpc


type ('query,'result) t = {
  rpc: ('query,'result Or_error.t) Rpc.t;
  log: ('query, 'result Oci_Log.line, Error.t) Pipe_rpc.t;
  forget: ('query,unit Or_error.t) Rpc.t;
  id : ('query * 'result) Type_equal.Id.t;
} [@@deriving fields]

exception NoResult

let register ~name ~version ~bin_query ~bin_result = {
  rpc = Rpc.create ~name ~version
      ~bin_query ~bin_response:(Or_error.bin_t bin_result);
  log = Pipe_rpc.create ~name:(name^" Oci.log") ~version
      ~bin_query
      ~bin_response:(Oci_Log.bin_line bin_result)
      ~bin_error:Error.bin_t
      ();
  forget = Rpc.create ~name:(name ^ " Oci.forget")
      ~version
      ~bin_query ~bin_response:(Or_error.bin_t Unit.bin_t);
  id = Type_equal.Id.create ~name sexp_of_opaque;
}

let name t = Rpc.name t.rpc
let version t = Rpc.version t.rpc
