#!/bin/bash
cd ${0%/*}

read -r HOSTNAME PRIVATE_IFACE PUBLIC_IFACE DISK_LOG_ROOT <<< $(cat ~/rtsv2_environment)
export PATH=/home/id3as/.nix-profile/bin:"$PATH"
export NIX_PROFILES="/nix/var/nix/profiles/default /home/id3as/.nix-profile"
export NIX_PATH=/home/id3as/.nix-defexpr/channels
export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

nix-shell --run "serf agent -iface $PRIVATE_IFACE -node $HOSTNAME -bind 127.0.0.1:7946 -rpc-addr 127.0.0.1:7373 | tee -a $DISK_LOG_ROOT/i-serf.log"
