# First step #

Present:

- layer efficient in memory (hardlink)
- userland container
- A sort of task is modelized as a function `'query -> 'result`, a
   task is the application (`'query` and `'result` must have a binprot
   type class defined (`with bin_type_class`)).

Absent:

- Database
- Multi-server
- master in a standalone program

## Components ##

There are five kind of components (4 components and a multiple of runners):

- one monitor, Oci_Monitor, which is the first program to run and that only launch
  the master and the runners when the master ask to. It is in no
  user namespace (it is a program like any other)
- a wrapper that execute programs inside their own user namespace.
  This is a separated program because it is hard to fork correctly
  inside a program that use Async. It is used only by the monitor
- one simple program `Oci_Simple_Exec` which is in a user namespace
  that contains all the users. It is used for the creation of the
  environment for the master or cleaning.
- one master, Oci_Artefact+specific user test (frama-c, zarith,
e-acsl, ocaml. It is run in a user namespace with the `superroot` as
`root`. It does:
    - ask the monitor to the runners when needed
    - keep track of which test have been run and save this information
      to disk
    - save the artefacts (directory in the filesystem)
    - add an artefacts inside a runner usernamespace
- one runner by task (eg. Frama-C commit abcdef12345 with ocaml 4.02.0, zarith 0.10, gui)

Oci use four different users:

- original user: The usual one, the one in the shell, eg. your user
- superroot: The first additional id given in `/etc/subuid`
- root: The second one
- user: The 1001th one


|         | original user | superroot | root | user |
|---------|:-------------:|:---------:|:----:|:----:|
| `Oci_Monitor`     |X| | | |
| wrapper           |X| | | |
| `Oci_Simple_Exec` |X|X|X|X|
| `Oci_Artefact`    | |X|X|X|
| runners           | | |X|X|

## Technique ##

For the artefacts adding a layer inside a usernamespace we use simply
hardlinks (fast and very simple). In order to forbid modification of these
file the owner is set to a superroot (just a user not present in the
usernamespace)

A proof of concept of the wrapper (usernamespace, chroot, binding of
/proc /dev, ...) is present as test in the master of extunix (on github).

For the communication we use Rpc from Async (Core). It is typed and
quite straigh forward. We use named unix socket for the communication,
simpler to pass to the runner than file descriptor.

## Structure ##

- `Oci_Common` is shared by the monitor, the masters and the runner.
It contains the typed RPC interface.
- `Oci_Data` is shared by the masters and the runners. It contains API
for registering sort of tasks
- `Oci_Master` is used only by the masters. It contains its API
- `Oci_Runner` is used only by the runners. It contains its API,
request creation of artifacts, linking or copy.
- `Oci_Artefact` is used only by the monitor for handling artifacts.

Not yet present:

- `Oci_Monitor` main part of the monitor
- `Oci_Wrapper` wrapper that creates usernamespace

<!--  LocalWords:  namespace
 -->
