#!/usr/bin/env sh
socketPath=$1
rtsv2-media-gateway $socketPath 1>>/tmp/rtsv2-media-gateway.log  2>>/tmp/rtsv2-media-gateway.err &
pid=$!
while read line ; do
  :
done
kill -KILL $pid
