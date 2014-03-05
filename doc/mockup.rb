## Mockups ##

* run frama-c on an examples with some options

```
let rule =
    new_rule "frama-c run"
     "Launch frama-c with a set of files with a given option"

let commit: Commit.t option =
    new_option ~rule "Frama-c commit"
    "specify the option of Frama-c"

let files: Files.Set.t option =
    new_option ~rule "Files to use"
    "specify the files that must be available for the test"
    
let cmdline: string array option =
    new_option ~rule "Command line"
    "specify the command line to use for running frama-c"

let run env = ...


let () = register ~rule run

```