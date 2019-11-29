#!/usr/bin/env bash

cd ${0%/*}

source ../../scripts/shared_functions.sh

function main {
  local -r session=$1
  tmux -L "$session" kill-session || true
  destroy_vlans
  destroy_serfs
}

main "$@"
