## Installation instructions ##

### System Requirements ###

- shadow (aka uidmap on Debian)
- cgmanager (optional)
- xpra (optional, useful to get access to a shell in the container when tests
go wrong)
- linux (>= 3.18)

### Installation with opam

There will be an opam package for oci. Thus, you will be able to install
it directly through opam with the following command:

```
opam install oci
```

or for the developpement version

```
opam pin add oci --kind=git "https://github.com/bobot/oci.git#master"
```

### Installation without opam ###
#### Package requirements ####
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
and each package. OCI loves to use bleeding edge stuff. Note that these
packages might not be installed at their latest version after an
`opam install pkg`: they have complex interdependencies and the whole
JaneStreet libraries are sometimes
re-split in a new set of packages in a manner that can confuse
opam. **Ensure that `opam upgrade` does not tell you that some packages have
not been upgraded because of dependencies before continuing.** This can be
done by explicitly asking `opam upgrade pkg` to tell that you truly want the
latest version of `pkg`. Repeat that for all packages until
`opam upgrade` stays silent.

#### Compilation step

You can customize some part of the compilation process by adding a `.config`
file in the top directory of oci. It is included by `Makefile` if it exists.
In particular, you can set the `PREFIX` variable to an appropriate path if
you don't want to install the binaries under `/usr/local/bin`. Libraries will
be handled by `ocamlfind`.

- `make`
- `make install`

### Tests
- You don't need to install oci for the simple tests
- Be sure that your kernel can provide unprivileged usernamespaces:
`sysctl kernel.unprivileged_userns_clone=1` as root if
needed
- In the repository create the directory for the permanent database and the containers
temporary directories
```
mkdir test-oci-data
```
- In the repository:
```
bin/Oci_Monitor.native --binaries bin --binaries bin-test --master bin-test/tests_master.native --oci-data test-oci-data
```

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

- launch a new monitor

```shell
  oci_monitor \
      --oci-data=/path/to/data \
      --binaries=INSTALLED_LIB/bin \
      --master=INSTALLED_LIB/bin/oci_default_master \
```

- Get a list of available rootfs from lxc, and download an appropriate one
  (defaults to debian jessie amd64)
  - `bf_client list-download-rootfs`
  - `bf_client download-rootfs --socket OCI_DATA/oci.socket [rootfs-opts]`
  where `rootfs-opts` can be chosen among `--arch`, `--distribution` and
  `--release` if you're not satisfied with default ones. This should get you
  the ID of the created rootfs (typically `0`)
- Add necessary packages (for usual ocaml package with gui) to the initial rootfs:
  ```shell
  bf_client add-package --rootfs ID --socket OCI_DATA/oci.socket \
  autotools-dev binutils-dev libiberty-dev libncurses5-dev pkg-config \
  zlib1g-dev git gcc build-essential m4 autoconf time libgmp-dev xpra tmux \
  strace xterm libexpat1-dev libgmp-dev libgnomecanvas2-dev libgtk2.0-dev \
  libgtksourceview2.0-dev m4 ncurses-dev xsltproc libxml2-utils
  ```
  This will give you a new rootfs of id `NID` (typically `1`).
- Optional: check that the rootfs is known to the monitor:
  `bf_client list-rootfs --socket OCI_DATA/oci.socket NID` where
  `NID` is the ID you have retrieved at previous step. Note that if you do not
  provide an ID, nothing will be output.
- launch a specific test, e.g. the ones for frama-c:

```shell
  bf_client \
     run \
     --rootfs NID \
     --socket OCI_DATA/oci.socket \
     frama-c
```

- If things go wrong, you can use the following commands to get an xterm on
  the corresponding container:

```shell
  bf_client \
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
