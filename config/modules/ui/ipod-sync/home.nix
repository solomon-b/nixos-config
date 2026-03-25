{ pkgs, ... }:

let
  ipod-sync = pkgs.writeShellScriptBin "ipod-sync" ''
    set -euo pipefail

    NAS_SRC="192.168.5.6:/mnt/tank/Media/IPOD/"
    DEFAULT_MOUNT="/run/media/solomon/IPOD"
    MOUNT_POINT="''${1:-$DEFAULT_MOUNT}"
    DRY_RUN=""

    usage() {
      echo "Usage: ipod-sync [--dry-run] [MOUNT_POINT]"
      echo ""
      echo "Sync music from NAS to iPod (Rockbox)."
      echo ""
      echo "Options:"
      echo "  --dry-run    Show what would be transferred without doing it"
      echo "  -h, --help   Show this help message"
      echo ""
      echo "Arguments:"
      echo "  MOUNT_POINT  iPod mount point (default: $DEFAULT_MOUNT)"
      exit 0
    }

    # Parse arguments
    MOUNT_POINT=""
    for arg in "$@"; do
      case "$arg" in
        --dry-run)
          DRY_RUN="--dry-run"
          ;;
        -h|--help)
          usage
          ;;
        *)
          MOUNT_POINT="$arg"
          ;;
      esac
    done

    MOUNT_POINT="''${MOUNT_POINT:-$DEFAULT_MOUNT}"

    if ! mountpoint -q "$MOUNT_POINT"; then
      echo "Error: $MOUNT_POINT is not mounted. Plug in your iPod and try again."
      exit 1
    fi

    echo "Syncing from $NAS_SRC to $MOUNT_POINT"
    [ -n "$DRY_RUN" ] && echo "(dry run)"

    ${pkgs.rsync}/bin/rsync -avh --delete $DRY_RUN "$NAS_SRC" "$MOUNT_POINT/"
  '';
in
{
  home.packages = [ ipod-sync ];
}
