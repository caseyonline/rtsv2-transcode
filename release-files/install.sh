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
sudo rtsv2/bin/install_service.sh
