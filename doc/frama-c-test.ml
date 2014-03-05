
let rule =
    new_rule "frama-c run"
     "Launch frama-c with a set of files with a given option"

let commit: Commit.t option =
    new_option ~rule "Frama-c commit"
    "specify the option of Frama-c"

let files: Dir.t option =
    new_option ~rule "Files to use"
    "specify the files that must be available for the test"

let cmdline: string array option =
    new_option ~rule "Command line"
    "specify the command line to use for running frama-c"

let run env =
  install_machine () >>= fun () ->
  FramaC.install (Env.get commit env) >>=
  fun layer_framac ->
  FS.add_layer File.root layer_framac >>= fun () ->
  FS.mk_tmp_dir >>= fun tmpdir ->
  FS.add_dir tmpdir (Env.get files env) >>= fun () ->
  exec "frama-c" (Env.get cmdline env) >>
  function
  | Ok -> ...
  | Error -> ...


let () = register ~rule run
