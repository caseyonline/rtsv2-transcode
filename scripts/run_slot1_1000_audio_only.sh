#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full

cd ${0%/*}

source ./config.sh
which ffmpeg
ffmpeg \
  -re \
  -i \
  "${MEDIA_FILE_1000}" \
  -map 0:a \
  -ar 48000 \
  -b:a 128k \
  -acodec aac \
  -profile:a aac_he_v2 \
  -bf 0 \
  -tune zerolatency \
  -f flv "rtmp://"${INGEST_NODE}":"${INGEST_PORT}"/mmddev001/slot1ao_1000 pubUser=user pubPasswd=password"
