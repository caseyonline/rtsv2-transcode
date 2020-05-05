declare MEDIA_FILE_500=
declare MEDIA_FILE_1000=
declare VIDEO_CODEC=h264
declare VIDEO_PRESET=faster

case "${USER}" in
  stears)
    # MEDIA_FILE_500=../assets/stargate-nobframes.ts
    MEDIA_FILE_500=../assets/tos-inverse.ts
    MEDIA_FILE_1000=../assets/tos-abr.ts

    case "$(cat /etc/hostname)" in
      stdarwin)
        VIDEO_CODEC=h264_nvenc
        VIDEO_PRESET=fast
        ;;
      *)
        ;;
    esac
    ;;
  *)
    MEDIA_FILE=../../../media_samples/video/stargate-no-bframes__h264-8bit-720x400pvariable_mp2-stereo_1h41m37s.ts
    MEDIA_FILE_500=${MEDIA_FILE}
    MEDIA_FILE_1000=${MEDIA_FILE}
    ;;
esac

export MEDIA_FILE_500
export MEDIA_FILE_1000
export VIDEO_CODEC
export VIDEO_PRESET
