#!/usr/bin/env sh
socketPath=$1
transmitQueueCount=$2
receiveQueueCount=$3
transmitQueueCapacity=$4

mkdir -p "$(dirname "${socketPath}")"

logPath=$(dirname "${socketPath}")/$(basename "${socketPath}" ".sock").log
errPath=$(dirname "${socketPath}")/$(basename "${socketPath}" ".sock")-error.log

RUST_BACKTRACE=1 \
  rtsv2-media-gateway \
    --control-socket-path "${socketPath}" \
    --transmit-queue-count "${transmitQueueCount}" \
    --receive-queue-count "${receiveQueueCount}" \
    --transmit-queue-capacity "${transmitQueueCapacity}" \
    1>>"${logPath}"  \
    2>>"${errPath}" &

pid=$!
while read -r _ ; do
  :
done
kill -KILL $pid
