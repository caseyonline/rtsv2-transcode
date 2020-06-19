#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full

cd ${0%/*}

source ./config.sh

echo starting egest of "${PROTOCOL:-rtmp}"://"${1:-172.16.171.3}":"${2:-1937}"/00000000-0000-0000-0000-000000000001/high to "${3:-/tmp/test_rtsv2_low.ts}" 

ffmpeg \
  -i \
  "${PROTOCOL:-rtmp}"://"${1:-172.16.171.3}":"${2:-1937}"/00000000-0000-0000-0000-000000000001/high "${3:-/tmp/test_rtsv2_low.ts}" > /tmp/ffmpeg-egest.log 2>&1 &
