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
    local sysConfig=$1
    local basePort=$2
    local serfBind=$(($2 + 1000))
    local serfRpc=$(($2 + 2000))

    serf agent -bind=0.0.0.0:$serfBind -rpc-addr=127.0.0.1:$serfRpc -node=$1 > /tmp/$1.serf 2>&1 &

    erl -pa _build/default/lib/*/ebin -boot _build/default/rel/rtsv2/releases/1/rtsv2 -config test/config/$1 -detached -rtsv2 mode dev -rtsv2 id rtsv2TestRunner

    wait_for_server
}


main "$@"
