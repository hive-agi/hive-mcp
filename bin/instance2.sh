#!/usr/bin/env bash
# ============================================================================
# bin/instance2.sh: a lightweight, pokeable SECOND hive-mcp beside the live one
# ----------------------------------------------------------------------------
# Instance 2 is where the reloadable-core seam is exercised: reload hive-mcp's
# own source, hot reload addons, evict and re-activate them, reload the core
# while addons are mounted. None of that may happen on the live server, whose
# nREPL (7910) serves every session.
#
# Isolation, reused from bin/test-sandboxed.sh:
#   * HOME, XDG roots and the JVM's user.home point into a sandbox dir, and its
#     config.edn is dev/test-sandbox.config.edn (every live service disabled or
#     redirected to a closed port) with dev/instance2.overlay.edn merged over it.
#   * The JVM's working directory IS the sandbox, so cwd-relative files
#     (.nrepl-port, data/swarm/...) land there and never over the live ones.
#   * Integrant profile :instance2 (dev/hive/profiles/instance2.edn) drops every
#     transport the live server owns.
#
# Ports: nREPL 7950 (drive it), MCP over HTTP 7951 (read it as a client does).
#
# Usage:
#   bin/instance2.sh start [addon-repo ...]   # default addons below
#   bin/instance2.sh stop
#   bin/instance2.sh status
#   bin/instance2.sh log
#
# Several instances may run side by side: INSTANCE names the sandbox, and
# NREPL_PORT / HTTP_PORT pick its ports (defaults: instance2, 7950, 7951).
#   INSTANCE=probe-a NREPL_PORT=7960 HTTP_PORT=7961 bin/instance2.sh start
#
# Addons are sibling repositories mounted as :local/root, so hive-hot can
# reload them. Default: hive-compose hive-rss hive-guard (small, core deps only).
#
# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
FLEET_DIR="$(cd "$PROJECT_DIR/../.." 2>/dev/null && pwd)"
# A worktree lives at <repo>/.claude/worktrees/<name>; its siblings are the
# fleet's. Otherwise the fleet is the project's parent.
case "$PROJECT_DIR" in
  */.claude/worktrees/*) FLEET_DIR="$(cd "${PROJECT_DIR%%/.claude/worktrees/*}/.." && pwd)" ;;
  *)                     FLEET_DIR="$(cd "$PROJECT_DIR/.." && pwd)" ;;
esac
TEMPLATE="$PROJECT_DIR/dev/test-sandbox.config.edn"
OVERLAY="$PROJECT_DIR/dev/instance2.overlay.edn"

INSTANCE="${INSTANCE:-instance2}"
[[ "$INSTANCE" =~ ^[a-z0-9][a-z0-9-]*$ ]] || { echo "instance2: FATAL: INSTANCE must be [a-z0-9-]" >&2; exit 2; }
NREPL_PORT="${NREPL_PORT:-7950}"
HTTP_PORT="${HTTP_PORT:-7951}"
LIVE_PORTS=(7910 7911 7912 9998 9999)
TMPROOT="${TMPDIR:-/tmp}"; TMPROOT="${TMPROOT%/}"
SBX="$TMPROOT/hive-mcp-$INSTANCE"
PIDFILE="$SBX/instance2.pid"
LOGFILE="$SBX/instance2.log"
DEFAULT_ADDONS=(hive-compose hive-rss hive-guard)

die()  { echo "instance2: FATAL: $*" >&2; exit 2; }
note() { echo "instance2: $*" >&2; }

running_pid() {
  [[ -f "$PIDFILE" ]] || return 1
  local pid; pid="$(cat "$PIDFILE")"
  kill -0 "$pid" 2>/dev/null && echo "$pid"
}

port_busy() { (exec 3<>"/dev/tcp/127.0.0.1/$1") 2>/dev/null; }

cmd_status() {
  if pid="$(running_pid)"; then
    note "running pid=$pid nrepl=$NREPL_PORT http=$HTTP_PORT sandbox=$SBX"
    port_busy "$NREPL_PORT" && note "nREPL $NREPL_PORT: listening" || note "nREPL $NREPL_PORT: not yet listening"
  else
    note "not running"; return 1
  fi
}

cmd_stop() {
  local pid
  if pid="$(running_pid)"; then
    kill "$pid"
    for _ in $(seq 1 30); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
    kill -0 "$pid" 2>/dev/null && kill -9 "$pid"
    note "stopped pid=$pid"
  else
    note "not running"
  fi
  # Delete ONLY our own sandbox, proven by its exact path.
  if [[ -d "$SBX" && "$SBX" == "$TMPROOT/hive-mcp-$INSTANCE" && "$SBX" != "${HOME:-}" ]]; then
    rm -rf "$SBX"
  fi
}

cmd_start() {
  running_pid >/dev/null && die "already running (bin/instance2.sh status)"
  [[ -f "$TEMPLATE" && -f "$OVERLAY" ]] || die "missing $TEMPLATE or $OVERLAY"
  command -v clojure >/dev/null || die "clojure CLI not on PATH"
  command -v bb >/dev/null      || die "bb not on PATH (merges the config overlay)"
  for p in "$NREPL_PORT" "$HTTP_PORT"; do
    for live in "${LIVE_PORTS[@]}"; do [[ "$p" != "$live" ]] || die "port $p is a live-server port"; done
    port_busy "$p" && die "port $p already in use"
  done

  local addons=("$@"); [[ ${#addons[@]} -gt 0 ]] || addons=("${DEFAULT_ADDONS[@]}")
  local deps=""
  for a in "${addons[@]}"; do
    [[ -f "$FLEET_DIR/$a/deps.edn" ]] || die "addon repo not found: $FLEET_DIR/$a"
    deps+=" io.github.hive-agi/$a {:local/root \"$FLEET_DIR/$a\"}"
  done

  local real_home="${HOME:?HOME must be set}"
  rm -rf "$SBX"; mkdir -p "$SBX/.config/hive-mcp" "$SBX/.local/share" "$SBX/.cache" "$SBX/run" "$SBX/state" "$SBX/tmp"
  chmod 700 "$SBX/run"
  for c in .m2 .gitlibs .clojure; do [[ -e "$real_home/$c" ]] && ln -s "$real_home/$c" "$SBX/$c"; done

  # config.edn = sandbox template, overlay deep-merged over it (as EDN, never as text).
  # The per-instance ports are merged last, so the overlay never pins them.
  bb -e '(let [[t o nrepl http] *command-line-args*
               deep (fn deep [a b] (if (and (map? a) (map? b)) (merge-with deep a b) b))
               ports {:services {:nrepl {:port (parse-long nrepl)} :mcp-http {:port (parse-long http)}}}]
           (prn (reduce deep [(clojure.edn/read-string (slurp t)) (clojure.edn/read-string (slurp o)) ports])))' \
     <(sed "s#__SANDBOX__#$SBX#g" "$TEMPLATE") "$OVERLAY" "$NREPL_PORT" "$HTTP_PORT" > "$SBX/.config/hive-mcp/config.edn"
  chmod 600 "$SBX/.config/hive-mcp/config.edn"

  # Classpath resolved IN the project (deps.edn + :dev + the addon roots).
  local cp
  cp="$(cd "$PROJECT_DIR" && clojure -Sdeps "{:deps {$deps}}" -Spath -M:dev)" || die "classpath resolution failed"
  # clojure -Spath prints project-relative entries (src, dev, resources ...);
  # the JVM runs from the sandbox, so make them absolute.
  cp="$(tr ':' '\n' <<<"$cp" | while read -r e; do [[ "$e" == /* ]] && echo "$e" || echo "$PROJECT_DIR/$e"; done | paste -sd: -)"

  (
    cd "$SBX"
    export HOME="$SBX" XDG_CONFIG_HOME="$SBX/.config" XDG_DATA_HOME="$SBX/.local/share" \
           XDG_CACHE_HOME="$SBX/.cache" XDG_RUNTIME_DIR="$SBX/run" \
           GITLIBS="$real_home/.gitlibs" CLJ_CONFIG="$real_home/.clojure" \
           HIVE_KG_DB_PATH="$SBX/state/kg-datahike" HIVE_KG_DH_BACKEND=memory HIVE_KG_BACKEND=datascript \
           HIVE_PROFILE=instance2 HIVE_MCP_NREPL_PORT="$NREPL_PORT" HIVE_MCP_ADDON_LIFECYCLE=1 \
           HIVE_MCP_HTTP_ENABLED=true HIVE_MCP_HTTP_PORT="$HTTP_PORT" HIVE_MCP_HTTP_BIND=127.0.0.1
    nohup java -cp "$cp" \
      -Duser.home="$SBX" -Djava.io.tmpdir="$SBX/tmp" -Dhive.kg.backend=datascript \
      -Xmx2g -XX:+ExitOnOutOfMemoryError -XX:+UseG1GC -Djdk.attach.allowAttachSelf=true \
      --add-opens=java.base/jdk.internal.misc=ALL-UNNAMED --add-opens=java.base/java.nio=ALL-UNNAMED \
      clojure.main -e "(require 'hive-mcp.server.core)
                       (hive-mcp.server.core/start! :profile :instance2)
                       (println \"instance2: READY instance=$INSTANCE nrepl=$NREPL_PORT http=$HTTP_PORT\")
                       @(promise)" \
      >"$LOGFILE" 2>&1 &
    echo $! > "$PIDFILE"
  )
  note "started pid=$(cat "$PIDFILE") sandbox=$SBX log=$LOGFILE addons=${addons[*]}"
  note "wait for 'instance2: READY' in the log, then drive it on nREPL $NREPL_PORT"
}

case "${1:-}" in
  start)  shift; cmd_start "$@" ;;
  stop)   cmd_stop ;;
  status) cmd_status ;;
  log)    exec tail -n 60 "$LOGFILE" ;;
  *)      sed -n '2,30p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'; exit 1 ;;
esac
