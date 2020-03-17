#!/usr/bin/env bash

cd ${0%/*}

source ../../scripts/shared_functions.sh

function main {
  local -r session=$1
  ./stopSession.sh $session
  sleep 1
  create_net
  tmux -L "$session" -2 new-session -d -s "$session"
  sleep 0.5
}



main "$@"
