#!/bin/sh
# https://unix.stackexchange.com/questions/216919/how-can-i-make-my-user-services-wait-till-the-network-is-online/249609#249609
host="${1:-8.8.8.8}"

pingcheck() {
  ping -n -c 1 -w 5 $1 >/dev/null 2>&1
}

# Do you want a timeout ?
while :; do
  pingcheck ${host} && exit 0
  sleep 10
done
