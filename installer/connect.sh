#!/usr/bin/env bash

SSID=$(gum input --placeholder "enter ssid..")
PSK=$(gum input --placeholder "enter password..")
echo "Choose Interface:"
INTERFACE=$(basename -a /sys/class/net/* | gum choose)

wpa_passphrase -B -i "${INTERFACE}" -c <(wpa_passphrase "$SSID" "$PSK")
