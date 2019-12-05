#!/usr/bin/env bash

cd ${0%/*}
cd ..

function main {
  serf leave \
       -rpc-addr $HOSTNAME:8373 >> logs/$HOSTNAME/t-serf.log
}

main "$@"
