#!/bin/sh -eux



sudo cgm create all oci
sudo cgm chown all oci $(id -u) $(id -g)
cgm movepid all oci $PPID
