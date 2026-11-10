# Hermes runs this script before each run of the homelab-sre cron job. It
# wakes the agent only when findings.json has a finding at err or higher that
# the previous run did not see. Hermes reads only the last stdout line as the
# gate. All stdout goes to the agent as context.
#
# A non-zero exit wakes the agent with a Script Error block. The job prompt
# tells the agent to report that failure.
set -euo pipefail

findings="${SRE_FINDINGS:-/var/lib/friendly-ghost/structured/findings.json}"
seen="${HERMES_HOME:-/var/lib/hermes/.hermes}/sre-seen.json"
max_age=1800

fail() {
  echo "sre-gate: $1" >&2
  exit 1
}

[ -r "$findings" ] || fail "cannot read $findings"

age=$(($(date +%s) - $(stat -c %Y "$findings")))
[ "$age" -le "$max_age" ] || fail "findings.json is ${age}s old"

jq -e '.schema_version | tostring | startswith("1")' "$findings" >/dev/null ||
  fail "unsupported schema_version"

status=$(jq -r '.status' "$findings")
[ "$status" != "failed" ] || fail "friendly-ghost status is failed"

current=$(jq -c '[.findings[]
  | select(.severity | IN("emerg", "alert", "crit", "err"))
  | .id] | unique' "$findings")
previous=$(jq -c 'if type == "array" then . else [] end' "$seen" 2>/dev/null || echo '[]')
new=$(jq -nc --argjson c "$current" --argjson p "$previous" '$c - $p')

# Keep only the ids that are in the file now. A finding that leaves the file
# and comes back wakes the agent again. The ids are saved before the agent
# runs, so a failed agent run does not see them again.
printf '%s\n' "$current" >"$seen.tmp"
mv "$seen.tmp" "$seen"

if [ "$new" = "[]" ]; then
  echo '{"wakeAgent": false}'
  exit 0
fi

# Leave out the summary text. Log text can trip the Hermes prompt-injection
# scan, and that scan blocks the whole run.
echo "friendly-ghost status: $status"
jq -c --argjson new "$new" '.findings[]
  | select(.id | IN($new[]))
  | {id, host, unit, category, severity, first_seen, last_seen, occurrences}' \
  "$findings"
echo '{"wakeAgent": true}'
