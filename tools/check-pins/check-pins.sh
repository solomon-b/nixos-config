#!/usr/bin/env bash
# Find every dockerTools.pullImage block in config/ and report drift against
# the upstream registry. Intentional logic:
#   - Floating tag (latest, stable, beta, ...): pin is drifted if the upstream
#     digest for the same tag no longer matches.
#   - Semver-shape tag (v2.25.0, 2026.6.1, ...): pin is outdated if a newer
#     semver tag exists upstream.
#   - Anything else (or missing finalImageTag): UNKNOWN, since we can't reason
#     about it without more context. Tubearchivist's archivist-es and
#     archivist-redis pins, for example, are missing finalImageTag — that's a
#     real config issue worth surfacing.

set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$REPO_ROOT"

FLOATING_TAGS_RE='^(latest|stable|beta|master|main|edge|nightly)$'
SEMVER_TAGS_RE='^v?[0-9]+(\.[0-9]+){1,3}$'

parse_pins() {
  # Parses mkPin { image = "name:tag"; imageDigest = "sha256:..."; ... } blocks.
  # Brace-depth tracked so that nested attrs (environment, etc.) inside the
  # mkPin call don't close the outer block prematurely. The tag is taken as
  # everything after the LAST `:` in the image string, so registry-with-port
  # references parse correctly.
  find config -name '*.nix' -type f | while read -r f; do
    awk '
      function emit(    _n, _i, _ci) {
        if (image == "") return
        _n = length(image)
        _ci = 0
        for (_i = _n; _i >= 1; _i--) {
          if (substr(image, _i, 1) == ":") { _ci = _i; break }
        }
        if (_ci > 0) {
          print FILENAME "\t" substr(image, 1, _ci - 1) "\t" digest "\t" substr(image, _ci + 1)
        } else {
          print FILENAME "\t" image "\t" digest "\t"
        }
      }
      BEGIN { in_block = 0 }
      /mkPin[[:space:]]*\{/ && !in_block {
        in_block = 1; depth = 0; image = ""; digest = ""
      }
      in_block {
        depth += (split($0, _arr, "{") - 1) - (split($0, _arr, "}") - 1)
      }
      in_block && /^[[:space:]]*imageDigest[[:space:]]*=/ {
        if (match($0, /"[^"]+"/)) digest = substr($0, RSTART + 1, RLENGTH - 2)
      }
      in_block && /^[[:space:]]*image[[:space:]]*=/ {
        if (match($0, /"[^"]+"/)) image = substr($0, RSTART + 1, RLENGTH - 2)
      }
      in_block && depth == 0 {
        emit(); in_block = 0
      }
    ' "$f"
  done
}

semver_newer() {
  # 0 if $1 > $2 (semver), 1 otherwise.
  local a="${1#v}" b="${2#v}"
  [[ "$a" == "$b" ]] && return 1
  [[ "$(printf '%s\n%s\n' "$a" "$b" | sort -V | tail -n1)" == "$a" ]]
}

check_one() {
  local name="$1" digest="$2" tag="$3"

  if [[ -z "$tag" ]]; then
    printf 'UNKNOWN\tmissing finalImageTag\n'
    return
  fi

  if [[ "$tag" =~ $FLOATING_TAGS_RE ]]; then
    local current
    current=$(skopeo inspect "docker://${name}:${tag}" 2>/dev/null \
      | jq -r '.Digest // empty')
    if [[ -z "$current" ]]; then
      printf 'UNKNOWN\tskopeo inspect failed\n'
    elif [[ "$current" == "$digest" ]]; then
      printf 'CURRENT\t%s\n' "$tag"
    else
      printf 'DRIFTED\t%s → %s\n' "${digest:7:12}" "${current:7:12}"
    fi
    return
  fi

  if [[ "$tag" =~ $SEMVER_TAGS_RE ]]; then
    local newest
    newest=$(skopeo list-tags "docker://${name}" 2>/dev/null \
      | jq -r '.Tags[]' \
      | grep -E "$SEMVER_TAGS_RE" \
      | sort -V | tail -n1)
    if [[ -z "$newest" ]]; then
      printf 'UNKNOWN\tno semver tags found upstream\n'
    elif ! semver_newer "$newest" "$tag"; then
      printf 'CURRENT\t%s\n' "$tag"
    else
      printf 'OUTDATED\t%s → %s\n' "$tag" "$newest"
    fi
    return
  fi

  printf 'UNKNOWN\tunrecognized tag style: %s\n' "$tag"
}

mapfile -t pins < <(parse_pins)

if [[ ${#pins[@]} -eq 0 ]]; then
  gum style --foreground 1 "No dockerTools.pullImage blocks found in config/"
  exit 1
fi

gum style --bold "Checking ${#pins[@]} container pins..."
echo

stale=0

for line in "${pins[@]}"; do
  IFS=$'\t' read -r _file imageName digest tag <<<"$line"
  short_name="${imageName##*/}"

  result=$(check_one "$imageName" "$digest" "$tag")
  status="${result%%$'\t'*}"
  detail="${result#*$'\t'}"

  case "$status" in
    CURRENT)  color=2; icon="✓" ;;
    DRIFTED)  color=3; icon="⚡"; stale=$((stale + 1)) ;;
    OUTDATED) color=1; icon="⚠"; stale=$((stale + 1)) ;;
    *)        color=8; icon="?"; stale=$((stale + 1)) ;;
  esac

  icon_styled=$(gum style --foreground "$color" "$icon")
  status_styled=$(gum style --foreground "$color" --bold "$status")
  short_digest="${digest:7:12}"
  [[ -z "$short_digest" ]] && short_digest="-"
  [[ -z "$tag" ]] && tag="-"

  if [[ "$status" == "CURRENT" ]]; then
    printf '%s %-26s %-12s %s  %s\n' \
      "$icon_styled" "$short_name" "$tag" "$short_digest" "$status_styled"
  else
    printf '%s %-26s %-12s %s  %s — %s\n' \
      "$icon_styled" "$short_name" "$tag" "$short_digest" "$status_styled" "$detail"
  fi
done

echo
total=${#pins[@]}
if [[ "$stale" -eq 0 ]]; then
  gum style --bold --foreground 2 "All ${total} pins are current."
else
  gum style --bold --foreground 3 "${stale} of ${total} pins need attention."
  exit 1
fi
