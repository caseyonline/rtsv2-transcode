#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full

cd ${0%/*}

source ./config.sh

declare INGEST_PORT=${2:-1935}

ffmpeg \
  -re \
  -i \
  "${MEDIA_FILE_1000}" \
  -ar 48000 \
  -acodec aac \
  -vcodec "${VIDEO_CODEC}" \
  -g 25 \
  -preset "${VIDEO_PRESET}" \
  -b 0.5M \
  -vf drawtext="fontfile=monofonto.ttf: fontsize=96: box=1: boxcolor=black@0.75: boxborderw=5: fontcolor=white: x=(w-text_w)/2: y=((h-text_h)/2)+((h-text_h)/4): text='LOW A %{gmtime\:%H\\\\\:%M\\\\\:%S}'" \
  -bf 0 \
  -f flv "rtmp://"$1":"$INGEST_PORT"/mmddev001/slot1_500 pubUser=user pubPasswd=password" > /tmp/ffmpeg.log 2>&1 &
