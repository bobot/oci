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


let () =
  never_returns begin
    Oci_Runner.run
      ~implementations:[
        Rpc.Rpc.implement
          (Oci_Data.rpc Test_succ.test_succ)
          (fun _ q -> return (q + 1));
        Rpc.Rpc.implement
          (Oci_Data.rpc Test_succ.test_fibo)
          (fun conn q ->
             match q with
             | q when q < 0 -> return Int.min_value
             | 0 -> return 1
             | 1 -> return 1
             | q ->
               let q_1 = Oci_Runner.dispatch
                   conn Test_succ.test_fibo (q-1) in
               let q_2 = Oci_Runner.dispatch
                   conn Test_succ.test_fibo (q-2) in
               Deferred.both q_1 q_2
               >>= fun (q_1,q_2) ->
               return (q_1 + q_2))

      ]
  end
