#!/usr/bin/env bash

cd ${0%/*}

function main {
  local -r session=$1
  ./stopSession.sh $session
  tmux -L "$session" -2 new-session -d -s "$session"
}



main "$@"
