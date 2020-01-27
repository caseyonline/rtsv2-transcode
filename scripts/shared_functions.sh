function interface_name_prefix() {
  case "$(uname -s)" in
    "Darwin")
      # NOTE: these have to be named this way to work on macOS
      echo "vlan"
      ;;

    "Linux")
      echo "rtsv2-if"
      ;;
  esac
}

function interface_name_from_index {
  local -r interface_index=${1}
  echo "$(interface_name_prefix)${interface_index}"
}

function create_net {
  case "$(uname -s)" in
    "Darwin")
      ;;

    "Linux")
      sudo modprobe dummy
      sudo ip link add rtsv2-br type bridge
      ;;
  esac
}

function create_iface {
  local -r name=$1
  local -r addr=$2

  case "$(uname -s)" in
    "Darwin")
      sudo ifconfig "$name" create
      sudo ifconfig "$name" inet "$addr" netmask 255.255.255.255
      sudo route add -host "$addr" -interface "$name" > /dev/null
      ;;

    "Linux")
      sudo ip link add $name type dummy
      sudo ip addr add $addr/24 dev $name
      sudo ip link set $name up
      sudo ip link set $name master rtsv2-br
      sudo ip link set $name multicast on
      ;;
  esac
}

function destroy_net {
  case "$(uname -s)" in
    "Darwin")
      for i in $(ifconfig | grep "$(interface_name_prefix)"'[1-9][0-9][0-9]' | awk '{print $1}' | sed 's/://'); do
        sudo ifconfig "$i" destroy
      done

      for i in $(netstat -nr  | grep "$(interface_name_prefix)"'[1-9][0-9][0-9]' | awk '{print $1}' | sed 's/://'); do
        sudo route delete -host "$i" >/dev/null
      done
      ;;

    "Linux")
      for name in $(ip -j link | jq --arg INTERFACE_PREFIX "$(interface_name_prefix)" -r '.[].ifname | select(. | startswith($INTERFACE_PREFIX))'); do
        sudo ip link delete "${name}"
      done

      sudo ip link delete rtsv2-br || true
      ;;
  esac
}

function destroy_serfs {
  pkill -f 'serf.*172.16' || true
}

function destroy_beams {
  pkill -f 'rtsv2TestRunner' || true
}

function start_node {
  local -r tmuxSession=$1
  local -r nodeName=$2
  local -r iface=$3
  local -r addr=$4
  local -r sysConfig=$5

  create_iface "$iface" "$addr"

  mkdir -p "logs/$nodeName"
  touch "logs/$nodeName/t-serf.log"

  tmux -L "$tmuxSession" send-keys " export HOSTNAME=$addr" C-m
  tmux -L "$tmuxSession" send-keys " serf agent -iface $iface -node $nodeName -bind $addr:7946 -rpc-addr $addr:7373 | tee -a logs/$nodeName/i-serf.log | grep -v 'Accepted client'" C-m
  tmux -L "$tmuxSession" split-window -h -p 80
  tmux -L "$tmuxSession" send-keys " export HOSTNAME=$addr" C-m
  tmux -L "$tmuxSession" send-keys " tail -f logs/$nodeName/t-serf.log | grep -v 'Accepted client'" C-m
  tmux -L "$tmuxSession" split-window -h -p 50
  tmux -L "$tmuxSession" send-keys " export HOSTNAME=$addr" C-m
  tmux -L "$tmuxSession" split-window -h -p 50
  tmux -L "$tmuxSession" send-keys " export HOSTNAME=$addr" C-m
  tmux -L "$tmuxSession" send-keys " export PRIVATE_IFACE=$iface" C-m
  tmux -L "$tmuxSession" send-keys " export PUBLIC_IFACE=$iface" C-m
  tmux -L "$tmuxSession" send-keys " export DISK_LOG_ROOT=$(pwd)/logs/$nodeName" C-m
  tmux -L "$tmuxSession" send-keys " erl -pa _build/default/lib/*/ebin -config $sysConfig -rtsv2 id '\"rtsv2TestRunner$nodeName\"' -eval 'application:ensure_all_started(rtsv2).'" C-m
}

function stop_node {
  local -r tmuxSession=$1
  local -r nodeName=$2
  local -r addr=$3

  pkill -9 -f "rtsv2TestRunner$nodeName" || true
  serf leave --rpc-addr $addr:7373 > /dev/null
  pkill -9 -f "rpc-addr $addr:7373" || true
}
