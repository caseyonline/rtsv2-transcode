#!/usr/bin/env bash

set -x

cd ${0%/*}
cd ../..


# shellcheck source=../../scripts/shared_functions.sh
source ./scripts/shared_functions.sh

function wait_for_server {
  local -r addr=$1
  while (! curl -v  "http://$addr:3000/test/alive") # --silent --fail
  do
    sleep 0.5
  done
}

function main {
  local -r session=$1
  local -r nodeName=$2
  local -r vlan=$3
  local -r addr=$4
  local -r sysConfig=$5

  tmux -L "$session" new-window -n "$nodeName"
  start_node "$session" "$nodeName" "$vlan" "$addr" "$sysConfig"

  wait_for_server "$addr"
}


main "$@"
