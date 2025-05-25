#!/usr/bin/env bash

OPTIONS=$(cat <<'EOF'
1: Logout | loginctl terminate-session "$XDG_SESSION_ID"
2: Shutdown | systemctl poweroff
3: Reboot | systemctl reboot
EOF
)

CHOICE=$(echo "$OPTIONS" | cut -d'|' -f1 | dmenu -l 10 -nb '#2d2d2d' -nf '#cccccc' -sb '#333333' -sf '#cccccc' | sed 's/ *$//')
[ -z "$CHOICE" ] && exit 0

CMD=$(echo "$OPTIONS" | grep -F "$CHOICE" | cut -d'|' -f2- | head -n 1)
[ -n "$CMD" ] && eval "$CMD"
