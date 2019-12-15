#!/usr/bin/env bash

cd ${0%/*}

source ../../scripts/shared_functions.sh

function main {
  local -r session=$1
  tmux -L "$session" kill-session > /dev/null 2>&1 || true
  destroy_beams
  destroy_serfs
  destroy_vlans

}

main "$@"
