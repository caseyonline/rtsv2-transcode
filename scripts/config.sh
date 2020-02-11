declare MEDIA_FILE=
declare INGEST_NODE=172.16.171.5

case "${USER}" in
  stears)
    MEDIA_FILE=../assets/tos-abr.ts
    ;;
  *)
    MEDIA_FILE=../../media_samples/video/stargate-no-bframes__h264-8bit-720x400pvariable_mp2-stereo_1h41m37s.ts
    ;;
esac
