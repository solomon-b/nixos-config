#!/usr/bin/env bash

set -euo pipefail

MUSIC_ROOT="/mnt/media/Music"
POST_SCRIPT="postprocess-ipod"

declare -A albums

# Function to get tag value using ffprobe
get_tag() {
  ffprobe -v error -show_entries format_tags="$1" -of default=nw=1:nk=1 "$2" 2>/dev/null || echo ""
}

# Recursively find all audio files
find "$MUSIC_ROOT" -type f \( -iname "*.mp3" -o -iname "*.flac" -o -iname "*.aac" -o -iname "*.ogg" -o -iname "*.wav" \) | while read -r file; do
  artist=$(get_tag artist "$file")
  album=$(get_tag album "$file")

  # Fallbacks
  artist="${artist:-Unknown Artist}"
  album="${album:-Unknown Album}"

  key="$artist|$album"
  albums["$key"]+="$file|"
done

# For each album group, invoke the original script
for key in "${!albums[@]}"; do
  IFS='|' read -r artist album <<< "$key"
  added_paths="${albums[$key]}"
  added_paths="${added_paths%|}"  # trim trailing pipe

  echo "Processing Album: $artist - $album"
  LIDARR_ENV=(
    "lidarr_artist_name=$artist"
    "lidarr_album_title=$album"
    "lidarr_addedtrackpaths=$added_paths"
  )

  env "${LIDARR_ENV[@]}" "$POST_SCRIPT"
done
