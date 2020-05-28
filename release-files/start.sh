#!/bin/bash

set -e

cd ${0%/*}

ROOTDIR="$(pwd)"/..
LIBDIR="$ROOTDIR"/lib
BINDIR="$ROOTDIR"/bin
export NODE_NAME=rtsv2
export RELX_REPLACE_OS_VARS=true

read -r HOSTNAME PUBLIC_IFACE SUPPORT_IFACE SYSTEM_IFACE IS_PROXIED DISK_LOG_ROOT <<< $(cat /id3as/rtsv2_environment)
export HOSTNAME
export PUBLIC_IFACE
export SUPPORT_IFACE
export SYSTEM_IFACE
export IS_PROXIED
export DISK_LOG_ROOT
export CODE_LOADING_MODE=interactive
export PATH=$PATH:$BINDIR
export LD_LIBRARY_PATH=$LIBDIR/rtsv2-1.0/lib/id3as_media-1/priv

mkdir -p $DISK_LOG_ROOT

$BINDIR/rtsv2 foreground
