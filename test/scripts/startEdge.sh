#!/usr/bin/env bash

cd ${0%/*}
cd ../..
erl -pa _build/default/lib/*/ebin -boot _build/default/rel/rtsv2/releases/1/rtsv2 -name testEdge@127.0.0.1 -config release-files/sys.config -detached -rtsv2 mode dev
