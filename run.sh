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

# shellcheck source=/dev/null
source "$ENV_SCRIPT"
unset HISTFILE

SESSION=rtsv2
SYSCONFIG=${SYSCONFIG:-release-files/sys.config}

function create_vlan {
    local vlan=$1
    local addr=$2

    sudo ifconfig lo0 alias $addr netmask 255.255.255.0

    #sudo ifconfig "$vlan" create
    #sudo ifconfig "$vlan" inet "$addr" netmask 255.255.255.0
}

function destroy_vlans {
    for i in $(ifconfig lo0 | awk '/172.16/ {print $2}'); do
         sudo ifconfig lo0 -alias $i
    done

#    for i in $(ifconfig | grep 'vlan[1-9][0-9][0-9]' | awk '{print $1}' | sed 's/://'); do
#        sudo ifconfig "$i" destroy
#    done
}

function destroy_serfs {
    echo Destroy serfs
    pkill -f 'serf.*172.16' || true
}

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
        popIndex=0
        tmux -L "$SESSION" new-window -t $regionPopIndex -n "$currentRegionPop"
    fi

    vlan=vlan$((regionPopIndex * 100 + popIndex))
    create_vlan "$vlan" "$addr"

    if (( popIndex > 0 )); then
        tmux -L "$SESSION" split-window -v -p 50 -f
    fi
    tmux -L "$SESSION" send-keys "export HOSTNAME=$addr" C-m
    tmux -L "$SESSION" send-keys "serf agent -iface lo0 -node $currentRegionPop$popIndex -bind $addr:7946 -rpc-addr $addr:7373" C-m
    tmux -L "$SESSION" split-window -h -p 80
    tmux -L "$SESSION" send-keys "export HOSTNAME=$addr" C-m
    tmux -L "$SESSION" send-keys "serf agent -iface lo0 -node $currentRegionPop$popIndex -bind $addr:8946 -rpc-addr $addr:8373" C-m
    tmux -L "$SESSION" split-window -h -p 75
    tmux -L "$SESSION" send-keys "export HOSTNAME=$addr" C-m
    tmux -L "$SESSION" split-window -h -p 50
    tmux -L "$SESSION" send-keys "export HOSTNAME=$addr" C-m
    tmux -L "$SESSION" send-keys "erl -pa _build/default/lib/*/ebin -config "$SYSCONFIG" -eval 'application:ensure_all_started(rtsv2).'" C-m

    popIndex=$((popIndex + 1))
done

tmux -L "$SESSION" -2 attach-session

echo "Killing session and removing VLANs"
tmux -L "$SESSION" kill-session
destroy_vlans
destroy_serfs
