# OCI #

## Architecture ##

OCI is composed by three different components:

- one conductor which coordinate the work to do and maintain the global state
- many monitors who run on remote computers, execute the workers, take their output
  and save the artifacts
- many workers who sequentially execute a task and can depend on the
  outputs of other workers

## Difference with a Build System ##

- The input of a worker is completely fixed. So there is no sense to
  redo the same worker if something has changed since nothing change.
- Some outputs will never be forgetted, it is normally small data
  (if the test is succesfull, time taken, artifact)

## Dependencies ##
- The code is part of the description of a worker

## Outputs ##
- The output are ocaml-values which can be back of by files

## Examples ##

Example of a rule two files:
*
```INI

```

```ocaml
let rule = OCI.Rule.create
    ~id:OCI_id.id
    ~name:"Frama-c compilation"
    ~opamlib:["oci-git";"oci-compile"]
    ~lib:["compile.ml"]

let repo = OCI.Param.str ~rule "repo"
let commit = OCI.Param.str ~rule "commit"

let success = OCI.Output.bool ~rule ~permanent:true "success"
```
