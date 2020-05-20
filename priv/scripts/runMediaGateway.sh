#!/usr/bin/env sh
socketPath=$1
logPath=$(dirname "${socketPath}")/$(basename "${socketPath}" ".sock").log
errPath=$(dirname "${socketPath}")/$(basename "${socketPath}" ".sock")-error.log

RUST_BACKTRACE=1 rtsv2-media-gateway "${socketPath}" 1>>"${logPath}"  2>>"${errPath}" &
pid=$!
while read -r _ ; do
  :
done
kill -KILL $pid
