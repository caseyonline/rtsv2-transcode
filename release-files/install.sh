#!/bin/bash

set -e

cd ${0%/*}

cd ..

Wd=$(pwd)
InstallName="${Wd##*/}"

cd ..

if [[ -L rtsv2 ]]; then
    rm rtsv2
fi

ln -s "$InstallName" rtsv2

cd rtsv2/bin
nix-shell --run "echo Nix setup complete"
sudo ./install_services.sh
