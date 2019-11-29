#!/bin/bash
set -e

usage() {
  echo "Usage: ./${0} (stears | ashton | etc)"
}

if [ "$#" -ne 1 ]; then
  RUN_ENV=$USER
  echo "Defaulting to $RUN_ENV"
else
  RUN_ENV=$1
fi
ENV_SCRIPT=scripts/env/$RUN_ENV

source scripts/shared_functions.sh
# shellcheck source=/dev/null
source "$ENV_SCRIPT"
set +o history

SESSION=rtsv2
SYSCONFIG=${SYSCONFIG:-release-files/sys.config}


tmux -L "$SESSION" kill-session 2>/dev/null || true
destroy_serfs
destroy_vlans

tmux -L "$SESSION" -2 new-session -d -s "$SESSION"

regionPopIndex=0
currentRegionPop=""
popIndex=0

array=()
while IFS='' read -r line; do array+=("$line"); done < <(jq -r 'keys_unsorted[] as $region | .[$region] | keys[] as $pop | .[$pop] | values[] as $addr | [$region, $pop, $addr] | @csv' "$POP_DEFINITION" | sed 's/"//g' )

for i in "${array[@]}"; do

    IFS=, read -r region pop addr <<< "$i"

    if [[ $region-$pop != "$currentRegionPop" ]]; then
        echo "Preparing PoP $region / $pop"
        currentRegionPop=$region-$pop
        regionPopIndex=$((regionPopIndex + 1))
        popIndex=1
        tmux -L "$SESSION" new-window -t $regionPopIndex -n "$currentRegionPop"
    fi

    vlan=vlan$(((regionPopIndex * 100) + popIndex))

    if (( popIndex > 1 )); then
        tmux -L "$SESSION" split-window -v -p 50 -f
    fi
    start_node "$SESSION" "$currentRegionPop$popIndex" "$vlan" "$addr" "$SYSCONFIG"
    popIndex=$((popIndex + 1))
done

tmux -L "$SESSION" -2 attach-session

echo "Killing session and removing VLANs"
tmux -L "$SESSION" kill-session
destroy_vlans
destroy_serfs
