## Installation instructions ##

### Requirements ###

#### System utilities

- shadow (aka uidmap on Debian)
- cgmanager (optional)
- xpra (optional, useful to get access to a shell in the container when things
  go wrong)

#### OCaml
The following opam packages are required to compile the various components of
OCI:

- async_shell
- core
- core_extended
- extunix
- fileutils
- textutils
- ocamlbuild
- ppx_here
- ppx_fields_conv
- ppx_compare
- ppx_sexp_conv
- ppx_bin_prot

Of course, opam will take care of installing their dependencies as well.
Please ensure that you truly have the latest version available of OCaml
and each package. OCI loves to use bleeding edge stuff.


#### opam package

There is now an opam package for oci. Thus, you should be able to install
it directly through opam with the following command:
```
opam pin add oci --kind=git "git@git.frama-c.com:bobot/oci.git#master"
```

### Compilation step

- `make`

### Usage

- All commands and sub-commands mentioned below have a `--help` option.
  In what follows, only the most important options are provided. See
  the corresponding `--help` for more information.
- Be sure that your kernel can provide unprivileged usernamespaces:
`echo 1 > /proc/sys/kernel/unprivileged_userns_clone` as root if needed
- Optional: configure cgroups, needed for cpu partitionning. In the current shell:

```
sudo cgm create all oci
sudo cgm chown all oci $(id -u) $(id -g)
cgm movepid all oci $$
```

- Get the ssh key necessary to access Frama-C's gitlab repository (ask
  @bobot or @virgile)
- launch a new monitor

```shell
  bin/Oci_Monitor.native \
      --oci-data=/path/to/data \
      --binaries=$(pwd)/bin \
      --master=bin/myoci.native \
      --identity-file=oci-ssh-key
```

- Get a list of available rootfs from lxc, and download an appropriate one
  (defaults to debian jessie amd64)
  - `bin/bf_client.native list-download-rootfs`
  - `bin/bf_client.native download-rootfs --socket OCI_DATA [rootfs-opts]`
  where `rootfs-opts` can be chosen among `--arch`, `--distribution` and
  `--release` if you're not satisfied with default ones. This should get you
  the ID of the created rootfs (typically `1`)
- Optional: check that the rootfs is known to the monitor:
  `bin/bf_client.native list_rootfs --socket OCI_DATA/oci.socket ID` where
  `ID` is the ID you have retrieved at previous step. Note that if you do not
  provide an ID, nothing will be output.
- launch a specific test, e.g. the ones for frama-c:

```shell
  bin/bf_client.native \
     run \
     --rootfs ID \
     --socket OCI_DATA/oci.socket \
     frama-c
```

- If things go wrong, you can use the following commands to get an xterm on
  the corresponding container:

```shell
  bin/bf_client.native \
     xpra \
     --rootfs ID \
     --socket OCI_DATA/oci.socket \
     frama-c
```
The log will contain something like
```
[hh:mm:ss] Run locally: XPRA_SOCKET_HOSTNAME=oci xpra attach :100 --socket-dir "/bla/xpra_socket"
[hh:mm:ss] Run remotely: xpra attach --remote-xpra "/bla/xpra_socket/remote-xpra.sh" ssh:HOST:100
```
copy the appropriate command (on a machine where xpra itself is installed of course)

- If you want to restart a run on the exact same set of commits, 
first launch the `run` command with `OCIFORGET` environment variable set,
then launch it as usual. Note that this will only forget the repository asked
on the command line, not the ones it depends upon.
