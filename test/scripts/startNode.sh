#!/usr/bin/env bash

cd ${0%/*}
cd ../..


wait_for_server() {
  while (! curl --silent --fail http://localhost:3000/test/alive)
  do
    sleep 0.5
  done
}
main() {
  erl -pa _build/default/lib/*/ebin -boot _build/default/rel/rtsv2/releases/1/rtsv2 -config test/config/$1 -detached -rtsv2 mode dev -rtsv2 id rtsv2TestRunner
  wait_for_server
}


main "$@"
