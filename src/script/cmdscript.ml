
open Core.Std
open Cmdliner

module Arg = struct
  include Arg
  let exec = string
end

(* Implementations, just print the args. *)

type verb = Normal | Quiet | Verbose
type copts = { debug : bool; verb : verb}
type script  = {script_exec: string; script_args: string list} with sexp

let str = Printf.sprintf
let opt_str sv = function None -> "None" | Some v -> str "Some(%s)" (sv v)
let opt_str_str = opt_str (fun s -> s)
let verb_str = function
  | Normal -> "normal" | Quiet -> "quiet" | Verbose -> "verbose"

let pr_copts fmt copts = Format.fprintf fmt
    "debug = %b\nverbosity = %s\n"
    copts.debug (verb_str copts.verb)

let create_vm copts script = Format.printf
    "%a %a\n" pr_copts copts Sexp.pp (sexp_of_script script)

let add_layer copts list names = Format.printf
    "%alist = %b\n\nnames = %s\n"
    pr_copts copts list (String.concat ~sep:", " names)

(* Help sections common to all commands *)

let copts_sect = "COMMON OPTIONS"
let help_secs = [
 `S copts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
 `P "Use `$(mname) help environment' for help on environment variables.";
 `S "BUGS"; `P "Check bug reports at https://bts.frama-c.com.";]

(* Options common to all commands *)

let copts debug verb = { debug; verb }
let copts_t =
  let docs = copts_sect in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let verb =
    let doc = "Suppress informational output." in
    let quiet = Quiet, Arg.info ["q"; "quiet"] ~docs ~doc in
    let doc = "Give verbose output." in
    let verbose = Verbose, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [Normal] [quiet; verbose])
  in
  Term.(pure copts $ debug $ verb)

(* Commands *)

let vm_create_cmd =
  let script_args =
    let script =
      Arg.(required & (pos 0 (some exec)) None & info [] ~docv:"SCRIPT") in
    let args = Arg.(value & (pos_right 0 string) [] & info [] ~docv:"ARG") in
    Term.(pure (fun script_exec script_args -> {script_exec; script_args}) $
          script $ args) in
  let doc = "make the current directory a repository" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a new vm ..."] @ help_secs
  in
  Term.(pure create_vm $ copts_t $ script_args),
  Term.info "create-vm" ~sdocs:copts_sect ~doc ~man

let add_layer_cmd =
  let list =
    let doc = "List existing layers." in
    Arg.(value & flag & info ["-l"; "list"] ~doc)
  in
  let files = Arg.(non_empty & (pos_all string) [] & info [] ~docv:"NAME") in
  let doc = "add an existing layer on top of the previous readonly one" in
  let man =
    [`S "DESCRIPTION";
     `P "The layers are produced by all the modifications done \
         on top of readonly layers. TODO"] @ help_secs
  in
  Term.(pure add_layer $ copts_t $ list $ files),
  Term.info "add-layer" ~doc ~sdocs:copts_sect ~man

let default_cmd =
  let doc = "oci" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "oci" ~version:Version.version ~sdocs:copts_sect ~doc ~man

let cmds = [vm_create_cmd; add_layer_cmd]

let () = match Term.eval_choice default_cmd cmds with
| `Error _ -> exit 1 | _ -> exit 0
