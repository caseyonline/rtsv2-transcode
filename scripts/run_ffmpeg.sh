#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full
file=$1
ingestUrl=$2
# ffmpeg -re -i "$file" -ar 44100 -acodec aac -vcodec h264 -g 25 -preset medium -x264-params "sliced-threads=1:rc-lookahead=0" -vf drawtext="fontfile=monofonto.ttf: fontsize=96: box=1: boxcolor=black@0.75: boxborderw=5: fontcolor=white: x=(w-text_w)/2: y=((h-text_h)/2)+((h-text_h)/4): text='%{gmtime\:%H\\\\\:%M\\\\\:%S}'" -bf 0 -f flv "$ingestUrl pubUser=user pubPasswd=password"
ffmpeg -re -i "$file" -ar 48000 -acodec aac -vcodec copy -bf 0 -f flv "$ingestUrl pubUser=user pubPasswd=password"
