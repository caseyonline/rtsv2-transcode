#!/usr/bin/env bash

cd ${0%/*}
cd ../..

# set -x
# exec >>/tmp/start.txt
# exec 2>&1

# shellcheck source=../../scripts/shared_functions.sh
source ./scripts/shared_functions.sh

function main {
  local -r session=$1
  local -r nodeName=$2
  local -r vlan=$3
  local -r addr=$4
  local -r sysConfig=$5
  #echo $(date): start node $2
  tmux -L "$session" new-window -n "$nodeName"
  start_node "$session" "$nodeName" "$vlan" "$addr" "$sysConfig"
}


main "$@"
