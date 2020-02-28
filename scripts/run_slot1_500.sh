#!/usr/bin/env bash

cd ${0%/*}

source ./config.sh

./run_ffmpeg.sh "${MEDIA_FILE}" rtmp://"${INGEST_NODE}":1935/mmddev001/slot1_500
