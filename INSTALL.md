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
Please ensure that you truly have the latest version available of each
package. OCI loves to use bleeding edge stuff.

### Compilation step

- `make`

### Usage

- Be sure that your kernel can provide unprivileged usernamespaces:
`echo 1 > /proc/sys/kernel/unprivileged_userns_clone` as root if needed
- Get the ssh key necessary to access Frama-C's gitlab repository (ask
  @bobot, @byako or @virgile)
- Get an archive of an initial rootfs (preferably for Debian GNU/Linux 64 bit)
  with its metadata (e.g. `rootfs.tar.xz`, `meta.tar.xz`)
- launch a new monitor
    `bin/Oci_Monitor.native \
      --oci-data=/path/to/data \
      --binaries=$(pwd)/bin \
      --master=bin/myoci.native \
      --identity-file=oci-ssh-key`
- Create an initial rootfs (to be done only once):
  `bin/launch-test.native OCI_DATA/oci.socket rootfs meta.tar.xz root.tar.xz`
- Stop and restart the monitor, and checks that a rootfs has indeed be
  created: `bin/bf_client.native list_rootfs --socket OCI_DATA/oci.socket`
- launch a specific test, e.g. the ones for frama-c:
  `bin/bf_client.native \
     run \
     --rootfs 0 \
     --socket OCI_DATA/oci.socket \
     frama-c`
