#!/usr/bin/env bash

cd ${0%/*}
cd ../..

# shellcheck source=../../scripts/shared_functions.sh
source ./scripts/shared_functions.sh

function main {
  local -r session=$1
  local -r nodeName=$2
  local -r addr=$3

  stop_node "$session" "$nodeName" "$addr"
}

main "$@"
