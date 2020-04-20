#!/usr/bin/env bash
socketPath=$1
rtsv2-media-gateway $socketPath 1>>/tmp/rtsv2-media-gateway.log  2>>/tmp/rtsv2-media-gateway.err
