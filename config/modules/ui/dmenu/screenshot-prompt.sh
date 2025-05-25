#!/usr/bin/env bash

OPTIONS=$(cat <<EOF
1: Capture Screen | scrot -F /home/solomon/Public/screenshots/%Y-%m-%d:%H:%M:%s.png
2: Capture Selection | scrot -s -F /home/solomon/Public/screenshots/%Y-%m-%d:%H:%M:%s.png
3: Capture All Screens | scrot -m -F /home/solomon/Public/screenshots/%Y-%m-%d:%H:%M:%s.png
4: Capture with 3 second countdown | scrot -d 3 -c -F /home/solomon/Public/screenshots/%Y-%m-%d:%H:%M:%s.png
EOF
)

CHOICE=$(echo "$OPTIONS" | cut -d'|' -f1 | dmenu -l 10 -nb '#2d2d2d' -nf '#cccccc' -sb '#333333' -sf '#cccccc')
CMD=$(echo "$OPTIONS" | grep -F "$CHOICE" | cut -d'|' -f2-)
[ -n "$CMD" ] && eval "$CMD"
