## Installation instructions ##

### Requirements ###

#### System utilities

- shadow (aka uidmap on Debian)
- cgmanager (optional)

#### OCaml
The following opam packages are required to compile the various components of
OCI:

- async_shell
- core
- core_extended
- extunix
- fileutils
- textutils

Of course, opam will take care of installing their dependencies as well.
Please ensure that you truly have the latest version available of OCaml
and each package. OCI loves to use bleeding edge stuff.

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