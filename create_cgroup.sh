#!/bin/sh -eux



sudo cgm create cpuset oci
sudo cgm chown cpuset oci $(id -u) $(id -g)
cgm movepid cpuset oci $PPID
