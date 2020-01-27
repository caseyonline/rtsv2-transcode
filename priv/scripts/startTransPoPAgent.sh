#!/usr/bin/env bash

cd ${0%/*}
cd ..

function main {
  nohup serf agent \
        -iface $PRIVATE_IFACE \
        -node $HOSTNAME \
        -bind $HOSTNAME:8946 \
        -rpc-addr $HOSTNAME:8373 >> logs/$HOSTNAME/t-serf.log &
}
echo trans pop start $@, $PRIVATE_IFACE, $HOSTNAME

main "$@"
