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
  -vcodec "${VIDEO_CODEC}" \
  -g 25 \
  -preset "${VIDEO_PRESET}" \
  -b 1m \
  -vf drawtext="fontfile=monofonto.ttf: fontsize=96: box=1: boxcolor=black@0.75: boxborderw=5: fontcolor=white: x=(w-text_w)/2: y=((h-text_h)/2)+((h-text_h)/4): text='HIGH A %{gmtime\:%H\\\\\:%M\\\\\:%S}'" \
  -bf 0 \
  -f flv "rtmp://"$1":1935/mmddev001/slot1b_1000 pubUser=user pubPasswd=password" > /tmp/ffmpeg.log 2>&1 &
