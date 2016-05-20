#!/bin/sh -eux

OCIROOT=/sys/fs/cgroup/cpuset/oci

if test \! -e $OCIROOT; then sudo mkdir $OCIROOT; fi
sudo chown -R $(id -u):$(id -g) $OCIROOT
echo $PPID > $OCIROOT/cgroup.procs
