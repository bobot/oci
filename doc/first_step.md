# First step #

Present:
 * layer efficient in memory (hardlink)
 * userland container
 * A sort of task is modelized as a function `'query -> 'result`, a
   task is the application (`'query` and `'result` must have a binprot
   type class defined (`with bin_type_class`)).

Absent:
 * Database
 * Multi-server

## Components ##

There are four components:
* one main monitor which:
  * launch the wrapped masters and runners
  * save the artefacts (directory in the filesystem)
  * add an artefacts inside a runner usernamespace
* one master by sort of task (eg. one for Frama-C, one for E-ACSL, one
for ocaml, one for zarith, ...) can be generated automatically or can be specialized. It keep
track of which tasks of this sort are currently running, and it could
save its state on disk (sexp, binprot).
* one runner by task (eg. Frama-C commit abcdef12345 with ocaml 4.02.0, zarith 0.10, gui)
* wrappers that execute the masters and the runner inside their own usernamespace


The masters could be integrated inside the monitor but since we
already need rpc and usernamespace it is easier to compile them as
standalone programs.

## Technique ##

For the artefacts adding a layer inside a usernamespace we use simply
hardlinks (fast and very simple). In order to forbid modification of these
file the owner is set to a superroot (just a user not present in the
usernamespace)

A proof of concept of the wrapper (usernamespace, chroot, binding of
/proc /dev, ...) is present as test in the master of extunix (on github).

For the communication we use Rpc from Async (Core). It is typed and
quite straigh forward. We use named unix socket for the communication,
simpler to pass to the runner and the master than file descriptor.

## Structure ##
* `Oci_Common` is shared by the monitor, the masters and the runner.
It contains the typed RPC interface.
* `Oci_Data` is shared by the masters and the runners. It contains API
for registering sort of tasks
* `Oci_Master` is used only by the masters. It contains its API
* `Oci_Runner` is used only by the runners. It contains its API,
request creation of artifacts, linking or copy.
* `Oci_Artefact` is used only by the monitor for handling artifacts.

Not yet present:
* `Oci_Monitor` main part of the monitor
* `Oci_Wrapper` wrapper that creates usernamespace
