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


type 'a both =
  | Line of Oci_Log.line
  | Result of 'a Or_error.t with sexp, bin_io

type ('query,'result) t = {
  rpc: ('query,'result Or_error.t) Rpc.t;
  log: ('query, Oci_Log.line, Error.t) Pipe_rpc.t;
  both: ('query, 'result both, Error.t)
      Pipe_rpc.t;
  id : ('query * 'result) Type_equal.Id.t;
} with fields


let register ~name ~version ~bin_query ~bin_result = {
  rpc = Rpc.create ~name ~version
      ~bin_query ~bin_response:(Or_error.bin_t bin_result);
  log = Pipe_rpc.create ~name:(name^" Oci.log") ~version
      ~bin_query
      ~bin_response:Oci_Log.bin_line
      ~bin_error:Error.bin_t
      ();
  both = Pipe_rpc.create ~name:(name^" Oci.both") ~version
      ~bin_query
      ~bin_response:(bin_both bin_result)
      ~bin_error:Error.bin_t
      ();
  id = Type_equal.Id.create ~name sexp_of_opaque;
}

let name t = Rpc.name t.rpc
let version t = Rpc.version t.rpc
