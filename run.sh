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

source $ENV_SCRIPT

SESSION=rtsv2-${CLUSTER_NAME:-primary}

function create_vlan {
    local vlan=$1
    local addr=$2

    sudo ifconfig $vlan create
    sudo ifconfig $vlan inet $addr netmask 255.255.255.0
}

function destroy_vlans {
    for i in $(ifconfig | grep vlan[1-9][0-9][0-9] | awk '{print $1}' | sed 's/://'); do
        sudo ifconfig $i destroy
    done
}

tmux -L $SESSION kill-session 2>/dev/null || true
destroy_vlans

tmux -L $SESSION -2 new-session -d -s $SESSION -x 2000 -y 2000

index=1

array=( $(jq -r 'keys_unsorted[] as $region | (.[$region] | keys_unsorted[] as $pop | (.[$pop] | [$region, $pop, .subnet, .numNodes] | @csv))' $POP_DEFINITION | sed 's/"//g' ) )

for i in "${array[@]}"; do
    echo $i | {
        IFS=, read -r region pop subnet numNodes
        tmux -L $SESSION new-window -t $index -n $region-$pop
        for ((i = 0; i < numNodes; i++ )); do
            addr=${subnet%.0}.$i
            create_vlan vlan$((index * 100 + $i)) $addr
            if [[ $i > 0 ]]; then
                tmux -L $SESSION split-window -v
            fi
            tmux -L $SESSION send-keys "export HOSTNAME=$addr" C-m
        done
        tmux -L $SESSION select-layout even-vertical
    }
    index=$((index + 1))
done

tmux -L $SESSION -2 attach-session

echo "Killing session cos you detached"
tmux -L $SESSION kill-session
destroy_vlans
