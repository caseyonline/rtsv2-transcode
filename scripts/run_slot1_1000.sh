#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full

cd ${0%/*}

source ./config.sh

ffmpeg \
  -re \
  -i \
  "${MEDIA_FILE_1000}" \
  -ar 48000 \
  -b:a 128k \
  -acodec aac \
  -profile:a aac_he_v2 \
  -vcodec "${VIDEO_CODEC}" \
  ${VIDEO_PROFILE} \
  -g 25 \
  -preset "${VIDEO_PRESET}" \
  -b:v 1M \
  -maxrate 1M \
  -bufsize 0.25M \
  -vf drawtext="fontfile=monofonto.ttf: fontsize=80: box=1: boxcolor=black@0.75: boxborderw=5: fontcolor=white: x=(w-text_w)/2: y=((h-text_h)/2)+((h-text_h)/4): text='HIGH A %{gmtime\:%H\\\\\:%M\\\\\:%S}'" \
  -bf 0 \
  -tune zerolatency \
  -f flv "${PROTOCOL:-rtmp}://"${INGEST_NODE}":"${INGEST_PORT}"/${SHORT_NAME}/slot1_1000 pubUser=user pubPasswd=password"
