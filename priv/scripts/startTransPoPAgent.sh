#!/usr/bin/env bash

cd ${0%/*}
cd ..

function main {
  IP=$(ip -4  -o addr | grep \ $PRIVATE_IFACE\  | awk {'print $4'} | sed 's/\/.*$//')

  nohup serf agent \
        -iface $PRIVATE_IFACE \
        -node $HOSTNAME \
        -bind $IP:8946 \
        -rpc-addr $IP:8373 >> $DISK_LOG_ROOT/t-serf.log &
}

main "$@"
