#!/usr/bin/env bash

cd ${0%/*}

./run_ffmpeg_audio_only.sh ../../media_samples/video/stargate-no-bframes__h264-8bit-720x400pvariable_mp2-stereo_1h41m37s.ts rtmp://172.16.171.5:1935/mmddev001/slot1_500
