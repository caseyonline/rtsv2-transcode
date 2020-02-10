#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full
file=$1
ingestUrl=$2
ffmpeg -re -i "$file" -map 0 -map -0:v -ar 44100 -acodec aac -f flv "$ingestUrl pubUser=user pubPasswd=password"
