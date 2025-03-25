#!/usr/bin/env bash

set -euo pipefail

# Only handle the AlbumDownload event
if [[ "${lidarr_eventtype:-}" != "AlbumDownload" ]]; then
  echo "Skipping event '${lidarr_eventtype:-<none>}': only 'AlbumDownload' is handled."
  exit 0
fi

# --- CONFIGURABLE ---
DEST_ROOT="/mnt/media/Ipod"
# --------------------

# Lidarr environment variables
ARTIST="${lidarr_artist_name:-Unknown Artist}"
ALBUM="${lidarr_album_title:-Unknown Album}"
ADDED_PATHS="${lidarr_addedtrackpaths:-}"

DEST_FOLDER="${DEST_ROOT}/${ARTIST}/${ALBUM}"
mkdir -p "$DEST_FOLDER"

echo "Lidarr Post-Import Script"
echo "Artist: $ARTIST"
echo "Album: $ALBUM"
echo "Destination folder: $DEST_FOLDER"

# Sanitize filenames using iconv + sed
sanitize() {
  echo "$1" \
    | iconv -f utf-8 -t ascii//TRANSLIT 2>/dev/null \
    | sed 's/[^A-Za-z0-9._ -]/_/g'
}

# Split pipe-separated paths
IFS='|' read -ra TRACKS <<< "$ADDED_PATHS"
for SRC_FILE in "${TRACKS[@]}"; do
  if [[ ! -f "$SRC_FILE" ]]; then
    echo "Skipping missing file: $SRC_FILE"
    continue
  fi

  EXT="${SRC_FILE##*.}"
  BASENAME="$(basename "$SRC_FILE")"
  SANITIZED_FILENAME="$(sanitize "$BASENAME")"
  DEST_FILE="${DEST_FOLDER}/${SANITIZED_FILENAME}"

  if [[ "${EXT,,}" == "mp3" ]]; then
    echo "Copying MP3: $SRC_FILE → $DEST_FILE"
    cp -p "$SRC_FILE" "$DEST_FILE"
  else
    # Ensure .mp3 extension on output
    DEST_FILE_MP3="${DEST_FILE%.*}.mp3"
    echo "Converting to MP3: $SRC_FILE → $DEST_FILE_MP3"
    ffmpeg -i "$SRC_FILE" -y \
      -codec:a libmp3lame -b:a 320k -map_metadata 0 \
      "$DEST_FILE_MP3"
  fi
done

echo "Post-processing complete for: $ARTIST - $ALBUM"
exit 0
