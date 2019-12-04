#!/usr/bin/env bash

cd ${0%/*}
cd ../..

function wait_for_server {
  local -r addr=$1
  while (! curl --silent --fail  "http://$addr:3000/test/alive")
  do
    sleep 0.5
  done
}

function main {
  local -r addr=$1
  wait_for_server "$addr"

}
main "$@"
