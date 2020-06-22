#! /usr/bin/env nix-shell
#! nix-shell -i bash -p ffmpeg-full

ffprobe -v quiet -print_format json -show_streams $1
