(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2012   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

(*i $Id: pp.mli,v 1.22 2009-10-19 11:55:33 bobot Exp $ i*)

type formatter = Format.formatter
type 'a printer = formatter -> 'a -> unit

val print_option : 'a printer -> 'a option printer
val print_option_or_default :
  string -> 'a printer -> 'a option printer
val print_list :
  unit printer ->
  'a printer -> 'a list printer
val print_list_or_default :
  string -> unit printer ->
  'a printer -> 'a list printer
val print_list_par :
  (formatter -> unit -> unit) ->
  'b printer -> 'b list printer
val print_list_delim :
  start:unit printer ->
  stop:unit printer ->
  sep:unit printer ->
  'b printer -> 'b list printer

val print_pair_delim :
  unit printer ->
  unit printer ->
  unit printer ->
  'a printer ->
  'b printer -> ('a * 'b) printer
val print_pair :
  'a printer ->
  'b printer -> ('a * 'b) printer

val print_iter1 :
  (('a -> unit) -> 'b -> unit) ->
  unit printer ->
  'a printer ->
  'b printer

val print_iter2:
  (('a -> 'b -> unit) -> 'c -> unit) ->
  unit printer ->
  unit printer ->
  'a printer ->
  'b printer ->
  'c printer
(**  [print_iter2 iter sep1 sep2 print1 print2 fmt t]
     iter iterator on [t : 'c]
     print1 k sep2 () print2 v sep1 () print1  sep2 () ...
*)


val print_iteri2:
  (('a -> 'b -> unit) -> 'c -> unit) ->
  unit printer ->
  unit printer ->
  'a printer ->
  ('a -> 'b printer) ->
  'c printer
(**  [print_iter2 iter sep1 sep2 print1 print2 fmt t]
     iter iterator on [t : 'c]
     print1 k sep2 () print2 v sep1 () print1  sep2 () ...
*)

val print_iter22:
  (('a -> 'b -> unit) -> 'c -> unit) ->
  unit printer ->
  (formatter -> 'a -> 'b -> unit) ->
  'c printer
(**  [print_iter22 iter sep print fmt t]
     iter iterator on [t : 'c]
     print k v sep () print k v sep () ...
*)

(** formatted: string which is formatted "@ " allow to cut the line if
    too long *)
type formatted = (unit, unit, unit, unit, unit, unit) format6
val empty_formatted : formatted

val space : unit printer
val alt : unit printer
val alt2 : unit printer
val newline : unit printer
val newline2 : unit printer
val dot : unit printer
val comma : unit printer
val star : unit printer
val simple_comma : unit printer
val semi : unit printer
val colon : unit printer
val underscore : unit printer
val equal : unit printer
val arrow : unit printer
val lbrace : unit printer
val rbrace : unit printer
val lsquare : unit printer
val rsquare : unit printer
val lparen : unit printer
val rparen : unit printer
val lchevron : unit printer
val rchevron : unit printer
val nothing : 'a printer
val string : string printer
val float : float printer
val int : int printer
val constant_string : string -> unit printer
val formatted : formatted printer
val constant_formatted : formatted -> unit printer
val print0 : unit printer
val hov : int -> 'a printer -> 'a printer
val indent : int -> 'a printer -> 'a printer
(** add the indentation at the first line *)
val add_flush : 'a printer -> 'a printer

val asd : 'a printer -> 'a printer
(** add string delimiter  " " *)

val open_formatter : ?margin:int -> out_channel -> formatter
val close_formatter : formatter -> unit
val open_file_and_formatter : ?margin:int -> string -> out_channel * formatter
val close_file_and_formatter : out_channel * formatter -> unit
val print_in_file_no_close :
  ?margin:int -> (formatter -> unit) -> string -> out_channel
val print_in_file : ?margin:int -> (formatter -> unit) -> string -> unit


val print_list_opt :
  unit printer ->
  (formatter -> 'a -> bool) -> formatter -> 'a list -> bool


val string_of : 'a printer -> 'a -> string
val string_of_wnl : 'a printer -> 'a -> string
  (** same as {!string_of} but without newline *)

val wnl : formatter -> unit

val sprintf :
  ('b,  formatter, unit, string) Pervasives.format4 -> 'b

val sprintf_wnl :
  ('b,  formatter, unit, string) Pervasives.format4 -> 'b

val to_sexp: ('a -> Core.Std.Sexp.t) -> Format.formatter -> 'a -> unit

module Ansi :
sig
  val set_column : int printer
end
