
function create_vlan {
  local vlan=$1
  local addr=$2

  #sudo ifconfig lo0 alias $addr netmask 255.255.255.0

  sudo ifconfig "$vlan" create
  sudo ifconfig "$vlan" inet "$addr" netmask 255.255.255.255
  sudo route add -host "$addr" -interface "$vlan" > /dev/null
}

function destroy_vlans {
  for i in $(ifconfig | grep 'vlan[1-9][0-9][0-9]' | awk '{print $1}' | sed 's/://'); do
    sudo ifconfig "$i" destroy
  done

  for i in $(netstat -nr  | grep 'vlan[1-9][0-9][0-9]' | awk '{print $1}' | sed 's/://'); do
    sudo route delete -host "$i" >/dev/null
  done

}

function destroy_serfs {
  pkill -f 'serf.*172.16' || true
}


function start_node {
  local -r tmuxSession=$1
  local -r nodeName=$2
  local -r vlan=$3
  local -r addr=$4
  local -r sysConfig=$5

  create_vlan "$vlan" "$addr"

  tmux -L "$tmuxSession" send-keys " export HOSTNAME=$addr" C-m
  tmux -L "$tmuxSession" send-keys " serf agent -iface $vlan -node $nodeName -bind $addr:7946 -rpc-addr $addr:7373" C-m
  tmux -L "$tmuxSession" split-window -h -p 80
  tmux -L "$tmuxSession" send-keys " export HOSTNAME=$addr" C-m
  tmux -L "$tmuxSession" send-keys " serf agent -iface $vlan -node $nodeName -bind $addr:8946 -rpc-addr $addr:8373" C-m
  tmux -L "$tmuxSession" split-window -h -p 75
  tmux -L "$tmuxSession" send-keys " export HOSTNAME=$addr" C-m
  tmux -L "$tmuxSession" split-window -h -p 50
  tmux -L "$tmuxSession" send-keys " export HOSTNAME=$addr" C-m
  tmux -L "$tmuxSession" send-keys " export PRIVATE_IFACE=$vlan" C-m
  tmux -L "$tmuxSession" send-keys " erl -pa _build/default/lib/*/ebin -config $sysConfig -rtsv2 id rtsv2TestRunner -eval 'application:ensure_all_started(rtsv2).'" C-m
}
