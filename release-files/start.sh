#!/bin/bash

set -e

cd ${0%/*}

ROOTDIR="$(pwd)"/..
LIBDIR="$ROOTDIR"/lib
BINDIR="$ROOTDIR"/bin
export NODE_NAME=rtsv2
export RELX_REPLACE_OS_VARS=true
read -r HOSTNAME PRIVATE_IFACE PUBLIC_IFACE DISK_LOG_ROOT <<< $(cat ~/rtsv2_environment)
export HOSTNAME
export PRIVATE_IFACE
export PUBLIC_IFACE
export DISK_LOG_ROOT
$BINDIR/rtsv2 foreground
