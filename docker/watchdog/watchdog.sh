#!/bin/sh
# Container watchdog for the hive-mcp compose stack (CLJW-QDRANT-OOM).
#
# Docker's restart policy has two gaps this fills, for containers carrying
# the label $WATCHDOG_LABEL (default hive.watchdog.restart=true):
#
#   1. A running container that its healthcheck reports UNHEALTHY is never
#      restarted by docker. The watchdog restarts it.
#   2. Once docker's restart manager fails to start a container (seen as
#      'runc create failed: container with given ID already exists' after an
#      OOM kill), it gives up and the container stays exited. The watchdog
#      starts it again.
#
# A container stopped by hand (docker stop / compose stop) exits with
# OOMKilled=false and an empty State.Error, so the watchdog leaves it alone.
# It acts only after the same container is seen in the same bad state on two
# polls in a row. That gives docker's own restart manager the first chance.
set -u

LABEL="${WATCHDOG_LABEL:-hive.watchdog.restart=true}"
INTERVAL="${WATCHDOG_INTERVAL:-30}"
# At most one action per container per COOLDOWN seconds, so a container that
# keeps failing is not restarted in a tight loop (qdrant's start_period is 180s).
COOLDOWN="${WATCHDOG_COOLDOWN:-300}"
SEEN=/tmp/watchdog.seen
NEXT=/tmp/watchdog.next
LAST=/tmp/watchdog.last
: > "$SEEN"
: > "$LAST"

log() { echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) watchdog: $*"; }

log "watching label=$LABEL every ${INTERVAL}s"
while :; do
  : > "$NEXT"
  for id in $(docker ps -aq --filter "label=$LABEL" 2>/dev/null); do
    line=$(docker inspect -f '{{.Name}}|{{.State.Status}}|{{.State.OOMKilled}}|{{.State.ExitCode}}|{{if .State.Health}}{{.State.Health.Status}}{{end}}|{{.State.Error}}' "$id" 2>/dev/null) || continue
    name=$(echo "$line" | cut -d'|' -f1 | sed 's#^/##')
    status=$(echo "$line" | cut -d'|' -f2)
    oom=$(echo "$line" | cut -d'|' -f3)
    code=$(echo "$line" | cut -d'|' -f4)
    health=$(echo "$line" | cut -d'|' -f5)
    err=$(echo "$line" | cut -d'|' -f6-)

    action=""
    if [ "$status" = "running" ] && [ "$health" = "unhealthy" ]; then
      action=restart
    elif [ "$status" = "exited" ] || [ "$status" = "created" ] || [ "$status" = "dead" ]; then
      if [ "$oom" = "true" ] || [ -n "$err" ]; then
        action=start
      fi
    fi
    [ -z "$action" ] && continue

    key="$id:$action"
    echo "$key" >> "$NEXT"
    now=$(date +%s)
    last=$(grep "^$id " "$LAST" 2>/dev/null | cut -d' ' -f2)
    if [ -n "$last" ] && [ $((now - last)) -lt "$COOLDOWN" ]; then
      log "$name still $status/${health:-none}, in ${COOLDOWN}s cooldown since last $action"
      continue
    fi
    if grep -qx "$key" "$SEEN"; then
      grep -v "^$id " "$LAST" > "$LAST.tmp" 2>/dev/null; echo "$id $now" >> "$LAST.tmp"; mv "$LAST.tmp" "$LAST"
      log "$action $name (status=$status health=${health:-none} oom=$oom exit=$code error=${err:-none})"
      if docker "$action" "$id" >/dev/null 2>&1; then
        log "$action $name: ok"
      else
        log "$action $name: FAILED, will retry next poll"
      fi
    else
      log "$name looks stuck (status=$status health=${health:-none} oom=$oom exit=$code), acting next poll if unchanged"
    fi
  done
  mv "$NEXT" "$SEEN"
  sleep "$INTERVAL"
done
