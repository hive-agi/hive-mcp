#!/usr/bin/env bash
# bin/hive-mcp-launcher-test.sh: runs bin/hive-mcp itself with a stub
# `clojure` on PATH (no JVM) and checks its OOM wiring end to end:
#
#   1. supervised: the stub exits 137, 137, then 0 -> three launches, the
#      launcher exits 0, and with a systemd user manager each launch runs in
#      its own fresh hive-mcp-* scope (a fixed --unit name or a prefix built
#      once before the loop fails here).
#   2. supervised, final exit 7 -> the launcher exits 7 (a broken `exit $?`).
#   3. unsupervised: exec keeps the launcher's pid, the stub's exit status is
#      the launcher's, and with a user manager it runs in a hive-mcp-* scope.
#   4. HIVE_MCP_OOMD_PREFERENCE=off: systemd-run is never invoked, supervised
#      or not, and the stub stays in the caller's cgroup.
#   5. SIGTERM to the supervised launcher reaches the stub, and the launcher
#      exits 143.
#
#   bin/hive-mcp-launcher-test.sh     # one line per case, exits 1 on failure
#
# HIVE_MCP_LAUNCHER points the cases at another launcher (mutation checks).
#
# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later
# shellcheck disable=SC2016  # stub bodies are expanded by the stub
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LAUNCHER="${HIVE_MCP_LAUNCHER:-$SCRIPT_DIR/hive-mcp}"
# shellcheck source=bin/oom-policy.sh
source "$SCRIPT_DIR/oom-policy.sh"

pass=0; fail=0
ok()  { pass=$((pass + 1)); echo "ok   $*"; }
bad() { fail=$((fail + 1)); echo "FAIL $*"; }
expect() { if [[ "$2" == "$3" ]]; then ok "$1"; else bad "$1: want [$3] got [$2]"; fi; }

TMP="$(mktemp -d "${TMPDIR:-/tmp}/hive-mcp-launcher-test.XXXXXXXX")"
trap 'rm -rf "$TMP"' EXIT

# The stub clojure: launch n reads line n of $STUB_DIR/codes. A number is its
# exit status; "wait" traps TERM, says it is ready and idles until signalled.
mkdir -p "$TMP/bin" "$TMP/spy"
printf '%s\n' '#!/bin/bash' \
  'd="$STUB_DIR"' \
  'n=$(( $(cat "$d/n" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$d/n"' \
  'echo "$$" >> "$d/pids"' \
  'awk -F: '\''$1=="0"{print $3}'\'' /proc/$$/cgroup >> "$d/cgroups"' \
  'printf "%s\n" "$*" >> "$d/argv"' \
  'code="$(sed -n "${n}p" "$d/codes")"' \
  'if [ "$code" = wait ]; then' \
  '  trap '\''echo term >> "$d/signals"; exit 143'\'' TERM' \
  '  echo ready > "$d/ready"' \
  '  for _ in $(seq 600); do sleep 0.1; done; exit 99' \
  'fi' \
  'exit "${code:-0}"' > "$TMP/bin/clojure"
# A systemd-run spy for the "off" cases: logs any call, then fails.
printf '%s\n' '#!/bin/sh' 'echo "$*" >> "$SPY_LOG"' 'exit 1' > "$TMP/spy/systemd-run"
chmod +x "$TMP/bin/clojure" "$TMP/spy/systemd-run"

# Is a real systemd user scope available? (decides the scope assertions)
oom_scope_prefix hive-mcp-launcher-test none 2>/dev/null
scoped=""
(( ${#OOM_SCOPE_PREFIX[@]} > 0 )) && scoped=1
oom_scope_unstarted || true
[[ -z "$scoped" ]] && echo "skip scope assertions: no systemd user manager"
self_cg="$(awk -F: '$1=="0"{print $3}' /proc/$BASHPID/cgroup)"

# new_case <name> <code>...: a fresh stub dir with that exit sequence.
new_case() {
  local name="$1"; shift
  D="$TMP/$name"; mkdir -p "$D"
  printf '%s\n' "$@" > "$D/codes"
}
lines() { if [[ -f "$1" ]]; then wc -l < "$1"; else echo 0; fi; }

run_launcher() { # env-assignments come from the caller's `env` prefix
  STUB_DIR="$D" OOM_RESTART_DELAY=0 PATH="$TMP/bin:$PATH" \
    timeout 120 "$LAUNCHER" "$@" </dev/null >"$D/out" 2>"$D/err"
}

# 1. Supervised restarts after two OOM kills, a fresh scope per launch.
new_case sup 137 137 0
HIVE_MCP_SUPERVISE=1 HIVE_MCP_OOMD_PREFERENCE=avoid run_launcher --profile k8s-minimal x; rc=$?
expect "supervised: 137,137,0 -> three launches, exit 0" "rc=$rc launches=$(lines "$D/pids")" "rc=0 launches=3"
mapfile -t cgs < "$D/cgroups" 2>/dev/null
if [[ -n "$scoped" ]]; then
  uniq_n="$(printf '%s\n' "${cgs[@]}" | sort -u | wc -l)"
  scoped_n="$(printf '%s\n' "${cgs[@]}" | grep -c '/hive-mcp-[^/]*\.scope$')"
  expect "supervised: each launch in its own hive-mcp scope" "distinct=$uniq_n scoped=$scoped_n" "distinct=3 scoped=3"
fi
expect "supervised: --profile consumed, rest passed to clojure" \
  "$(head -1 "$D/argv" | grep -o -- '-m hive-mcp.server.core.*')" "-m hive-mcp.server.core x"

# 2. The supervisor's final status is the launcher's exit status.
new_case sup7 137 7
HIVE_MCP_SUPERVISE=1 HIVE_MCP_OOMD_PREFERENCE=off run_launcher; rc=$?
expect "supervised: 137 then 7 -> exit 7" "rc=$rc launches=$(lines "$D/pids")" "rc=7 launches=2"

# 3. Unsupervised: exec in place (same pid), exit status passed through.
new_case unsup 5
STUB_DIR="$D" HIVE_MCP_SUPERVISE=0 HIVE_MCP_OOMD_PREFERENCE=avoid PATH="$TMP/bin:$PATH" \
  "$LAUNCHER" </dev/null >"$D/out" 2>"$D/err" &
lpid=$!
( sleep 120; kill -KILL "$lpid" 2>/dev/null ) & guard=$!
wait "$lpid"; rc=$?
kill "$guard" 2>/dev/null; wait "$guard" 2>/dev/null
expect "unsupervised: one launch, exec keeps the pid, exit 5" \
  "rc=$rc launches=$(lines "$D/pids") pid=$(head -1 "$D/pids")" "rc=5 launches=1 pid=$lpid"
if [[ -n "$scoped" ]]; then
  case "$(head -1 "$D/cgroups")" in
    */hive-mcp-*.scope) ok "unsupervised: runs in a hive-mcp scope" ;;
    *) bad "unsupervised: cgroup [$(head -1 "$D/cgroups")] $(tail -3 "$D/err")" ;;
  esac
fi

# 4. OOMD_PREF=off: no systemd-run at all, the caller's cgroup.
for mode in 1 0; do
  new_case "off$mode" 0
  SPY_LOG="$D/spy" HIVE_MCP_SUPERVISE=$mode HIVE_MCP_OOMD_PREFERENCE=off \
    STUB_DIR="$D" OOM_RESTART_DELAY=0 PATH="$TMP/bin:$TMP/spy:$PATH" \
    timeout 120 "$LAUNCHER" </dev/null >"$D/out" 2>"$D/err"; rc=$?
  expect "off (supervise=$mode): no systemd-run, caller's cgroup, exit 0" \
    "rc=$rc spy=$(lines "$D/spy") cg=$(head -1 "$D/cgroups")" "rc=0 spy=0 cg=$self_cg"
done

# 5. SIGTERM to the supervised launcher reaches the running server.
new_case term wait
STUB_DIR="$D" HIVE_MCP_SUPERVISE=1 HIVE_MCP_OOMD_PREFERENCE=avoid OOM_RESTART_DELAY=0 \
  PATH="$TMP/bin:$PATH" "$LAUNCHER" </dev/null >"$D/out" 2>"$D/err" &
lpid=$!
for _ in $(seq 300); do [[ -f "$D/ready" ]] && break; sleep 0.1; done
kill -TERM "$lpid"
( sleep 30; kill -KILL "$lpid" 2>/dev/null ) & guard=$!
wait "$lpid"; rc=$?
kill "$guard" 2>/dev/null; wait "$guard" 2>/dev/null
spid="$(head -1 "$D/pids" 2>/dev/null)"
alive=no; [[ -n "$spid" ]] && kill -0 "$spid" 2>/dev/null && alive=yes
[[ "$alive" == yes ]] && kill -KILL "$spid" 2>/dev/null
expect "SIGTERM: stub got TERM, launcher exit 143, no orphan" \
  "rc=$rc signals=$(cat "$D/signals" 2>/dev/null) orphan=$alive launches=$(lines "$D/pids")" \
  "rc=143 signals=term orphan=no launches=1"

echo "hive-mcp-launcher-test: $pass passed, $fail failed"
(( fail == 0 ))
