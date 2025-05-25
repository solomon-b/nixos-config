#!/usr/bin/env bash

WIN_ID=$(xdotool getwindowfocus)
WIN_NAME=$(xprop -id "$WIN_ID" WM_NAME | cut -d '"' -f 2)

PROMPT="Kill window: $WIN_NAME"
echo "$PROMPT" | dmenu -l 1 -nb '#2d2d2d' -nf '#cccccc' -sb '#333333' -sf '#cccccc' >/dev/null && xkill -id "$WIN_ID"
