#!/usr/bin/env bash
set -eu
set -o pipefail

update_env_script() {
  ENV_SCRIPT=scripts/env/${ENV_NAME}
}

declare ENV_NAME=${1:-}
declare ENV_NAME_SPECIFIED="yes"
declare ENV_SCRIPT=

if [[ -z "${ENV_NAME}" ]]; then
  ENV_NAME="${USER}"
  ENV_NAME_SPECIFIED="no"
fi

update_env_script

if [[ ! -f "${ENV_SCRIPT}" ]]; then
  if [[ "${ENV_NAME_SPECIFIED}" == "yes" ]]; then
    echo "The explicitly specified environment ${ENV_NAME} was not found at ${ENV_SCRIPT}, aborting."
    return 64
  fi

  echo "No environment specified, and no user-specific environment found, falling back to the common environment."
  ENV_NAME=common
  update_env_script
fi

source scripts/shared_functions.sh
# shellcheck source=/dev/null
source "$ENV_SCRIPT"
set +o history

SESSION=rtsv2
SYSCONFIG=${SYSCONFIG:-release-files/sys.config}

tmux -L "$SESSION" kill-session 2>/dev/null || true
destroy_serfs
destroy_net

create_net

tmux -L "$SESSION" -2 new-session -d -s "$SESSION"

regionPopIndex=0
currentRegionPop=""
popIndex=0

array=()
while IFS='' read -r line; do array+=("$line"); done < <(jq -r 'keys_unsorted[] as $region | .[$region] | keys[] as $pop | .[$pop] | .nodes[] as $addr | [$region, $pop, $addr] | @csv' "$POP_DEFINITION" | sed 's/"//g' )

for i in "${array[@]}"; do

    IFS=, read -r region pop addr <<< "$i"

    if [[ $region-$pop != "$currentRegionPop" ]]; then
        echo "Preparing PoP $region / $pop"
        currentRegionPop=$region-$pop
        regionPopIndex=$((regionPopIndex + 1))
        popIndex=1
        tmux -L "$SESSION" new-window -t $regionPopIndex -n "$currentRegionPop"
    fi

    # NOTE: these have to be named in a particular way to work on macOS
    ifaceIndex=$(((regionPopIndex * 100) + popIndex))

    if (( popIndex > 1 )); then
        tmux -L "$SESSION" split-window -v -p 50 -f
    fi
    start_node "$SESSION" "$addr" "$ifaceIndex" "$addr" "$SYSCONFIG"
    popIndex=$((popIndex + 1))
done

tmux -L "$SESSION" -2 attach-session

echo "Killing session and removing interfaces"
tmux -L "$SESSION" kill-session
destroy_beams
destroy_serfs
destroy_net
