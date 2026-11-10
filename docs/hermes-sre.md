# Scheduled Hermes investigations

The `homelab-sre` cron job runs Hermes on caleuche every 15 minutes. Before
each run, `sre-gate.sh` reads the friendly-ghost findings. The agent wakes only
when a finding at `err` or higher is new since the previous run. When nothing
is new, Hermes makes no model call.

This is a small trial. It shows if a scheduled SRE is useful. It does not
retry failed runs, and it does not alert you if the job stops.

## Create the job

Deploy caleuche first. Then run this command on caleuche once. The `hermes`
wrapper runs the CLI as the hermes user.

```sh
hermes cron create '*/15 * * * *' "$(cat <<'EOF'
This is a scheduled homelab check. The script output above lists new
friendly-ghost findings at err or higher. Check each finding against Loki and
Prometheus with the rules in your instructions. Then file a Forgejo issue, or
add a comment to an open issue. Report only what a human operator would act
on.

If the output is a Script Error, the gate failed. Read the error. Check
findings.json and the friendly-ghost service on caleuche. Then file one issue
titled "caleuche: SRE gate failed", or add a comment to the open one.

End with a short summary of what you did.
EOF
)" --name homelab-sre --script sre-gate.sh \
  --provider openrouter --model deepseek/deepseek-v4.1-flash \
  --deliver local
```

## Use the job

- `hermes cron list` shows the job and its next run.
- `hermes cron runs homelab-sre` shows the runs. A gated run shows "agent
  skipped".
- `hermes cron pause homelab-sre` stops it. `hermes cron resume homelab-sre`
  starts it again.
- `hermes cron run homelab-sre` runs it on the next tick.

To see all current findings again, delete the seen list. The next run then
wakes for every finding at `err` or higher.

```sh
sudo -u hermes rm /var/lib/hermes/.hermes/sre-seen.json
```

## Known limits

- The gate saves the seen ids before the agent runs. If a run fails, the
  next run does not see those findings again. Delete the seen list to see
  them again.
- When findings.json is old or friendly-ghost fails, every run wakes the agent.
  The agent comments on the open "SRE gate failed" issue, so this costs one
  model call per run until you fix it.
- Nothing alerts you if the job stops. Check `hermes cron runs` now and then.
