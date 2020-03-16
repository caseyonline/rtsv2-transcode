declare MEDIA_FILE_500=
declare MEDIA_FILE_1000=
declare INGEST_NODE=${INGEST_NODE:-172.16.171.5}

case "${USER}" in
  stears)
    # MEDIA_FILE_500=../assets/stargate-nobframes.ts
    MEDIA_FILE_500=../assets/tos-inverse.ts
    MEDIA_FILE_1000=../assets/tos-abr.ts
    ;;
  *)
    MEDIA_FILE=../../media_samples/video/stargate-no-bframes__h264-8bit-720x400pvariable_mp2-stereo_1h41m37s.ts
    MEDIA_FILE_500=${MEDIA_FILE}
    MEDIA_FILE_1000=${MEDIA_FILE}
    ;;
esac

export MEDIA_FILE_500
export MEDIA_FILE_1000
export INGEST_NODE
