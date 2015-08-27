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
open Oci_Cmd_Runner_Api

let () =
  never_returns (
    Oci_Runner.run
      ~implementations:[
        Oci_Runner.implement run
          (fun _ d -> Async_shell.run d.prog d.args);
        Oci_Runner.implement create_artefact
          (fun t dir -> Oci_Runner.create_artefact t ~dir);
        Oci_Runner.implement link_to
          (fun t d -> Oci_Runner.link_artefact t d.artefact ~dir:d.dst);
        Oci_Runner.implement copy_to
          (fun t d -> Oci_Runner.copy_artefact t d.artefact ~dir:d.dst);
      ]
  )
