#!/usr/bin/env bash

cd ${0%/*}
cd ../..

# set -x
# exec >>/tmp/start.txt
# exec 2>&1

function wait_for_server {
  local -r addr=$1
  while true
  do
    IFS=, read -r intra trans transpop <<< $(curl --silent --fail http://$addr:3000/support/healthCheck | jq -r '[.intraPoPHealth, .transPoPHealth, .currentTransPoP] | @csv' | sed 's/"//g')
    #echo $addr: intraHealth $intra, transHealth: $trans
    if [[ "$intra" != "Perfect" ]]; then
        sleep 0.5
        continue
    fi

    if [[ "$trans" == "Perfect" ]]; then
        break
    fi

    if [[ "$trans" == "NA" ]]; then
         IFS=, read -r intra trans  <<< $(curl --silent --fail http://$transpop:3000/support/healthCheck | jq -r '[.intraPoPHealth, .transPoPHealth] | @csv' | sed 's/"//g')
         #echo $addr: trans: $transpop, transHealth: $trans
        if [[ "$trans" == "Perfect" ]]; then
            break
        fi

    fi
    sleep 0.5
  done
}

function main {
  local -r addr=$1
  wait_for_server "$addr"
  #echo $addr is ready
}

main "$@"
