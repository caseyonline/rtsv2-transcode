#!/bin/bash

set -e

if [[ $UID -ne 0 ]]; then
  echo "$0 must be run as root"
  exit 1
fi

cd ${0%/*}
BinDir=$(pwd)
RootDir=$(readlink -f $BinDir/../..)

systemctl stop rtsv2-serf.service 2>/dev/null || true
systemctl stop rtsv2-node.service 2>/dev/null || true

cat > /usr/lib/systemd/system/rtsv2-serf.service <<EOF
[Unit]
Description=RTS-V2 Serf
After=network.target

[Service]
Type=simple
User=id3as
WorkingDirectory=$RootDir
ExecStart=$BinDir/start_serf_nix.sh
Restart=always

[Install]
WantedBy=multi-user.target
EOF

systemctl enable rtsv2-serf.service

cat > /usr/lib/systemd/system/rtsv2-node.service <<EOF
[Unit]
Description=RTS-V2 Node
After=network.target rtsv2-serf.service
Requires=rtsv2-serf.service

[Service]
Type=simple
User=id3as
WorkingDirectory=$RootDir
ExecStart=$BinDir/start_node_nix.sh
ExecStop=$BinDir/rtsv2 stop
Restart=always
Environment=LD_LIBRARY_PATH=/id3as/rtsv2/lib/id3as_media-1/priv
Environment=DISK_LOG_ROOT=/id3as/logs

[Install]
WantedBy=multi-user.target
EOF

systemctl enable rtsv2-node.service
