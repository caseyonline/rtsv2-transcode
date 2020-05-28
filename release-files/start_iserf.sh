#!/usr/bin/env bash

set -e

cd ${0%/*}

read -r HOSTNAME PUBLIC_IFACE SUPPORT_IFACE SYSTEM_IFACE IS_PROXIED DISK_LOG_ROOT <<< $(cat /id3as/rtsv2_environment)

mkdir -p $DISK_LOG_ROOT

IP=$(ip -4  -o addr | grep \ $SYSTEM_IFACE\  | awk {'print $4'} | sed 's/\/.*$//')

serf agent -iface $SYSTEM_IFACE -node $HOSTNAME -bind $IP:7946 -rpc-addr $IP:7373 | tee -a $DISK_LOG_ROOT/i-serf.log
