#!/bin/sh -eux

OCIROOT=/sys/fs/cgroup/cpuset/oci

if test \! -e $OCIROOT; then sudo mkdir $OCIROOT; fi
sudo chown -R $(id -u):$(id -g) $OCIROOT
cat $OCIROOT/../cpuset.cpus > cat $OCIROOT/cpuset.cpus
cat $OCIROOT/../cpuset.mems > cat $OCIROOT/cpuset.mems
echo $PPID > $OCIROOT/cgroup.procs
