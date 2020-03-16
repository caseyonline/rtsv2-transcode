#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full

cd ${0%/*}

source ./config.sh

ffmpeg \
  -re \
  -i \
  "${MEDIA_FILE_1000}" \
  -ar 48000 \
  -acodec aac \
  -vcodec h264 \
  -g 25 \
  -preset faster \
  -b 500k \
  -vf drawtext="fontfile=monofonto.ttf: fontsize=96: box=1: boxcolor=black@0.75: boxborderw=5: fontcolor=white: x=(w-text_w)/2: y=((h-text_h)/2)+((h-text_h)/4): text='LOW %{gmtime\:%H\\\\\:%M\\\\\:%S}'" \
  -bf 0 \
  -f flv "rtmp://"${INGEST_NODE}":1935/mmddev001/slot1_500 pubUser=user pubPasswd=password"
