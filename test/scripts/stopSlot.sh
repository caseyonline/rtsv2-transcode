#!/usr/bin/env bash

function killffmpeg {
  pkill -f 'ffmpeg' || true
}

killffmpeg
