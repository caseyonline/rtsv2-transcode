#!/usr/bin/env bash

cd ${0%/*}
cd ..

function main {
  serf leave \
       -rpc-addr $HOSTNAME:8373 >> $DISK_LOG_ROOT/t-serf.log
}

main "$@"
