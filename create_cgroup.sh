#!/bin/sh -eux

CPUSET=/sys/fs/cgroup/cpuset/oci
CPUACCT=/sys/fs/cgroup/cpuacct/oci

if test \! -e $CPUSET; then sudo mkdir $CPUSET; fi
if test \! -e $CPUACCT; then sudo mkdir $CPUACCT; fi
sudo chown -R $(id -u):$(id -g) $CPUSET $CPUACCT
cat $CPUSET/../cpuset.cpus > cat $CPUSET/cpuset.cpus
cat $CPUSET/../cpuset.mems > cat $CPUSET/cpuset.mems
echo $PPID > $CPUSET/cgroup.procs
echo $PPID > $CPUACCT/cgroup.procs
