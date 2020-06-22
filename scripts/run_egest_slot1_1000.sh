#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full

cd ${0%/*}

source ./config.sh

ffmpeg \
  -i \
  "${PROTOCOL:-rtmp}"://"${1:-172.16.171.3}":"${2:-1937}"/00000000-0000-0000-0000-000000000001/high "${3:-/tmp/rtsv2_high.ts}"
