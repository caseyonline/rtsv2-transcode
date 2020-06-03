declare MEDIA_FILE_500=
declare MEDIA_FILE_1000=
declare INGEST_NODE=${INGEST_NODE:-172.16.171.3}
declare INGEST_PORT=${INGEST_PORT:-1935}
declare VIDEO_CODEC=${VIDEO_CODEC:-h264}
declare VIDEO_PRESET=faster
declare VIDEO_PROFILE=${VIDEO_PROFILE:-profile:v baseline}
declare SHORT_NAME=${SHORT_NAME:=mmddev001}

case "${USER}" in
  stears)
    # MEDIA_FILE_500=../_assets/stargate-nobframes.ts
    MEDIA_FILE_500=../_assets/tos-inverse.ts
    MEDIA_FILE_1000=../_assets/tos-abr.ts

    case "$(cat /etc/hostname)" in
      stdarwin)
        VIDEO_CODEC=h264_nvenc
        VIDEO_PRESET=fast
        ;;
      *)
        ;;
    esac
    ;;
  nicholaw)
    MEDIA_FILE=$HOME/id3as/media_samples/video/stargate-no-bframes__h264-8bit-720x400pvariable_mp2-stereo_1h41m37s.ts
    MEDIA_FILE_500=${MEDIA_FILE}
    MEDIA_FILE_1000=${MEDIA_FILE}

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
export VIDEO_CODEC
export VIDEO_PRESET
export VIDEO_PROFILE
export SHORT_NAME
