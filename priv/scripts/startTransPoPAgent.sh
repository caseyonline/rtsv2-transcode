#!/usr/bin/env bash

cd ${0%/*}
cd ..

function main {
  if [[ $(uname) == "Darwin" ]]; then
    IP=$(ifconfig $SYSTEM_IFACE | awk '/inet/ {print $2}')
  else
    IP=$(ip -4  -o addr | grep \ $SYSTEM_IFACE\  | awk {'print $4'} | sed 's/\/.*$//')
  fi

  echo serf agent \
        -iface $SYSTEM_IFACE \
        -node $HOSTNAME \
        -bind $IP:8946 \
        -rpc-addr $IP:8373 >> $DISK_LOG_ROOT/t-serf.log &

  nohup serf agent \
        -iface $SYSTEM_IFACE \
        -node $HOSTNAME \
        -bind $IP:8946 \
        -rpc-addr $IP:8373 >> $DISK_LOG_ROOT/t-serf.log &
}

main "$@"
