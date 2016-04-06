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

let test_succ _ q = return (q + 1)

let test_fibo conn q =
  match q with
  | q when q < 0 -> return Int.min_value
  | 0 -> return 1
  | 1 -> return 1
  | q ->
    Oci_Runner.release_proc conn 1
    >>= fun () ->
    let q_1 = Oci_Runner.dispatch_exn
        conn Tests_api.test_fibo (q-1) in
    let q_2 = Oci_Runner.dispatch_exn
        conn Tests_api.test_fibo (q-2) in
    Deferred.both q_1 q_2
    >>= fun (q_1,q_2) ->
    return (q_1 + q_2)

let test_collatz conn q =
  match q with
  | q when q < 0 -> return Int.min_value
  | 0 -> return 1
  | 1 -> return 1
  | q ->
    Oci_Runner.release_proc conn 1
    >>= fun () ->
    let collatz x =
      if (x mod 2) = 0
      then x/2
      else succ (x*3)
    in
    Oci_Runner.dispatch_exn
      conn Tests_api.test_collatz (collatz q)
    >>= fun r ->
    return (r + 1)

let test_fibo_artefact_aux conn q =
  let save_fibo v =
    Unix.mkdir "/fibo"
    >>= fun () ->
    Writer.open_file "/fibo/result"
    >>= fun writer ->
    Writer.write_bin_prot writer Int.bin_writer_t v;
    Writer.close writer
    >>= fun () ->
    Oci_Runner.create_artefact conn ~dir:"/fibo"
  in
  let read_fibo file =
    Reader.open_file file
    >>= fun reader ->
    Reader.read_bin_prot reader Int.bin_reader_t
    >>= function
    | `Eof -> invalid_arg "Bad fibo file"
    | `Ok r -> return r
  in
  match q with
  | q when q < 0 -> save_fibo Int.min_value
  | 0 -> save_fibo 1
  | 1 -> save_fibo 1
  | q ->
    Oci_Runner.release_proc conn 1
    >>= fun () ->
    Oci_Runner.dispatch_exn
      conn Tests_api.test_fibo_artefact_aux (q-1)
    >>= fun a_1 ->
    Oci_Runner.dispatch_exn
      conn Tests_api.test_fibo_artefact_aux (q-2)
    >>= fun a_2 ->
    Oci_Runner.get_proc conn 1
    >>= fun _ ->
    Oci_Runner.link_artefact conn a_1 ~dir:"/fibo_1"
    >>= fun () ->
    Oci_Runner.link_artefact conn a_2 ~dir:"/fibo_2"
    >>= fun () ->
    read_fibo "/fibo_1/result"
    >>= fun q_1 ->
    read_fibo "/fibo_2/result"
    >>= fun q_2 ->
    save_fibo (q_1 + q_2)

let test_fibo_artefact conn q =
  let read_fibo file =
    Reader.open_file file
    >>= fun reader ->
    Reader.read_bin_prot reader Int.bin_reader_t
    >>= function
    | `Eof -> invalid_arg "Bad fibo file"
    | `Ok r -> return r
  in
  Oci_Runner.release_proc conn 1
  >>= fun () ->
  Oci_Runner.dispatch_exn
      conn Tests_api.test_fibo_artefact_aux q
  >>= fun a ->
  Oci_Runner.get_proc conn 1
  >>= fun _ ->
  Oci_Runner.link_artefact conn a ~dir:"/fibo"
  >>= fun () ->
  read_fibo "/fibo/result"
  >>= fun q_2 ->
  return q_2



let test_fibo_error_artefact conn q =
  Oci_Runner.dispatch_exn
      conn Tests_api.test_fibo_artefact_aux q
  >>= fun a ->
  Oci_Runner.link_artefact conn a ~dir:"/fibo"
  >>= fun () ->
  Writer.open_file ~append:true "/fibo/result"
  >>= fun writer ->
  Writer.write_line writer "C'est une erreur!";
  Writer.close writer;
  >>= fun _ ->
  return (-1)

let () =
  never_returns begin
    Oci_Runner.start
      ~implementations:[
        Oci_Runner.implement
          Tests_api.test_succ test_succ;
        Oci_Runner.implement
          Tests_api.test_fibo test_fibo;
        Oci_Runner.implement
          Tests_api.test_fibo_artefact test_fibo_artefact;
        Oci_Runner.implement
          Tests_api.test_fibo_artefact_aux test_fibo_artefact_aux;
        Oci_Runner.implement
          Tests_api.test_fibo_error_artefact test_fibo_error_artefact;
        Oci_Runner.implement
          Tests_api.test_collatz test_collatz;
      ]
  end
