#!/usr/bin/env bash
set -x
set -eu
set -o pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

source scripts/shared_functions.sh

set +o history

SESSION=rtsv2
export SYSCONFIG=scripts/env/localhost.data/sys.config

tmux -L "$SESSION" kill-session 2>/dev/null || true
destroy_serfs

# To do at-scale tests with rtsv2-media-gateway, it's necessary
# to have a lot of a file-handles available
ulimit -n $(ulimit -H -n) 2>/dev/null || true

tmux -L "$SESSION" -2 new-session -d -s "$SESSION"

tmux -L "$SESSION" new-window -t 1 -n localhost

start_node "$SESSION" "localhost" any localhost "$SYSCONFIG"

tmux -L "$SESSION" -2 attach-session

echo "Killing session and removing interfaces"
tmux -L "$SESSION" kill-session
destroy_beams
destroy_serfs
