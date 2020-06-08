#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full

cd ${0%/*}

source ./config.sh
which ffmpeg
ffmpeg \
  -re \
  -i \
  "${MEDIA_FILE_1000}" \
  -map 0:v \
  -vcodec "${VIDEO_CODEC}" \
  ${VIDEO_PROFILE} \
  -g 25 \
  -preset "${VIDEO_PRESET}" \
  -b:v 500k \
  -vf drawtext="fontfile=monofonto.ttf: fontsize=80: box=1: boxcolor=black@0.75: boxborderw=5: fontcolor=white: x=(w-text_w)/2: y=((h-text_h)/2)+((h-text_h)/4): text='HIGH A %{gmtime\:%H\\\\\:%M\\\\\:%S}'" \
  -bf 0 \
  -tune zerolatency \
  -f flv "rtmp://"${INGEST_NODE}":"${INGEST_PORT}"/${SHORT_NAME}/slot1_500 pubUser=user pubPasswd=password"
