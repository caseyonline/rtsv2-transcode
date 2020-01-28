#!/bin/bash
cd ${0%/*}

export PATH=/home/id3as/.nix-profile/bin:"$PATH"
export NIX_PROFILES="/nix/var/nix/profiles/default /home/id3as/.nix-profile"
export NIX_PATH=/home/id3as/.nix-defexpr/channels
export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

nix-shell --run ./start.sh
