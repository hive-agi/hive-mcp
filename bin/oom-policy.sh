#!/usr/bin/env bash
# bin/oom-policy.sh — who dies first when the host runs out of RAM.
#
# Sourced by bin/hive-mcp (protect the server) and bin/test-sandboxed.sh
# (sacrifice the test JVM). Executed directly it is the proof:
#
#   bin/oom-policy.sh check <pid>     # adj, score, cgroup, oomd preference
#
# Why: hive-mcp died twice on 2026-10-17 while a 14 GB test JVM ate the host.
# Two different killers run here, and each needs its own lever:
#
#   1. The KERNEL OOM killer picks the process with the highest oom_score,
#      which oom_score_adj (-1000..1000) shifts. An unprivileged process may
#      only RAISE its own adj: lowering below the inherited value needs
#      CAP_SYS_RESOURCE (measured: `echo -100 > /proc/<pid>/oom_score_adj`
#      -> Permission denied). So the lever that always works is the inverse
#      one: test JVMs raise their own adj (default 800) and die first.
#      The server still TRIES to lower its own adj (HIVE_MCP_OOM_SCORE_ADJ,
#      default -500), which only succeeds when run with the capability.
#
#   2. systemd-oomd (the 15:34 and 16:01 kills that day) kills a whole
#      CGROUP when memory pressure on user@1000.service stays above 50 %
#      for 20 s. It ignores oom_score_adj. It does honour
#      ManagedOOMPreference=avoid when the unit's cgroup has the same owner
#      as the monitored ancestor, which holds for a `systemd-run --user
#      --scope` unit (measured: the scope gets xattr user.oomd_avoid=1).
#      The server therefore runs in its own transient scope with that
#      preference. It also stops sharing a cgroup with the terminal it was
#      started from, so oomd killing that terminal's scope no longer takes
#      the server with it.
#
# Every function degrades to a no-op with a message when its lever is not
# available (no systemd user manager, no /proc), never to a failure.
#
# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later

# oom_adj_set <value>: set THIS shell's oom_score_adj; children inherit it.
# Raising always works; lowering needs CAP_SYS_RESOURCE. Prints the outcome.
oom_adj_set() {
  # $BASHPID, not /proc/self inside a subshell: the write must land on THIS
  # shell (the one that later execs or forks the JVM), not on a throwaway child.
  local want="$1" have file="/proc/$BASHPID/oom_score_adj"
  have="$(cat "$file" 2>/dev/null)" || {
    echo "oom-policy: no $file, adj left alone" >&2; return 0; }
  if [[ "$have" == "$want" ]]; then
    return 0
  fi
  if { echo "$want" > "$file"; } 2>/dev/null; then
    echo "oom-policy: oom_score_adj $have -> $want (pid $$)" >&2
  else
    echo "oom-policy: oom_score_adj stays $have (lowering to $want needs CAP_SYS_RESOURCE)" >&2
  fi
  return 0
}

# oom_scope_prefix <name> <preference>: fill the array OOM_SCOPE_PREFIX with a
# prefix that runs the command in its own user scope carrying
# ManagedOOMPreference=<preference> (avoid|omit|none). Empty, with a message,
# when the preference is not one of those or when a real
# `systemd-run --user --scope -p ManagedOOMPreference=<preference> true`
# fails, so "${OOM_SCOPE_PREFIX[@]}" cmd... always runs cmd.
#
# systemd-run --scope execs the command in place: same pid, same stdio. The
# array holds only exec-ing programs (env, systemd-run, sh), never a shell
# function, so a launcher that backgrounds it and signals $! reaches the
# command itself.
#
# Call it once PER LAUNCH. Each call names a fresh unit and a fresh marker:
# a scope whose command died can stay loaded while its orphaned children
# live, and reusing its name makes the next systemd-run fail with "Unit ...
# already loaded". Even a fresh name can fail to start (manager gone, bus
# gone), so the prefix starts the command through a trampoline that deletes
# the marker file OOM_SCOPE_MARK; oom_scope_unstarted afterwards tells "the
# scope never started" (marker still there) apart from "the command ran and
# exited non-zero", and the launcher then runs the command bare.
#
# OOM_BUS_XDG_RUNTIME_DIR: the runtime dir systemd-run uses to reach the user
# manager, for callers that have pointed XDG_RUNTIME_DIR elsewhere (the test
# sandbox). The command still sees the caller's XDG_RUNTIME_DIR.
# shellcheck disable=SC2034  # read by the scripts that source this file
OOM_SCOPE_PREFIX=()
OOM_SCOPE_MARK=""
_OOM_SCOPE_SEQ=0
# shellcheck disable=SC2016  # expanded by the trampoline's sh, not here
_OOM_TRAMPOLINE='rm -f -- "$1"; if [ -n "$2" ]; then XDG_RUNTIME_DIR=$2; export XDG_RUNTIME_DIR; fi; shift 2; exec "$@"'

# shellcheck disable=SC2034
oom_scope_prefix() {
  local name="$1" pref="$2" bus=() restore=""
  OOM_SCOPE_PREFIX=()
  OOM_SCOPE_MARK=""
  case "$pref" in
    avoid|omit|none) ;;
    *) echo "oom-policy: ManagedOOMPreference '$pref' is not avoid|omit|none, $name runs in the caller's cgroup" >&2
       return 0 ;;
  esac
  if [[ -n "${OOM_BUS_XDG_RUNTIME_DIR:-}" ]]; then
    bus=(env XDG_RUNTIME_DIR="$OOM_BUS_XDG_RUNTIME_DIR")
    restore="${XDG_RUNTIME_DIR:-}"
  fi
  if ! command -v systemd-run >/dev/null 2>&1; then
    echo "oom-policy: no systemd-run, $name runs in the caller's cgroup" >&2
    return 0
  fi
  if ! timeout 10 ${bus[@]+"${bus[@]}"} systemd-run --user --scope --collect --quiet \
         -p "ManagedOOMPreference=${pref}" true >/dev/null 2>&1; then
    echo "oom-policy: systemd-run --user --scope -p ManagedOOMPreference=$pref failed, $name runs in the caller's cgroup" >&2
    return 0
  fi
  OOM_SCOPE_MARK="$(mktemp "${TMPDIR:-/tmp}/oom-scope.XXXXXXXX" 2>/dev/null)" || {
    OOM_SCOPE_MARK=""
    echo "oom-policy: no marker file, $name runs in the caller's cgroup" >&2
    return 0; }
  _OOM_SCOPE_SEQ=$((_OOM_SCOPE_SEQ + 1))
  OOM_SCOPE_PREFIX=(${bus[@]+"${bus[@]}"}
                    systemd-run --user --scope --collect --quiet
                    --unit "${name}-$$-${_OOM_SCOPE_SEQ}-${EPOCHSECONDS:-0}-${RANDOM}"
                    -p "ManagedOOMPreference=${pref}"
                    sh -c "$_OOM_TRAMPOLINE" oom-scope "$OOM_SCOPE_MARK" "$restore")
}

# oom_scope_unstarted: after a command launched through OOM_SCOPE_PREFIX has
# exited, succeed when its scope never started (the trampoline never ran).
# Always removes the marker. False when no scope was requested.
oom_scope_unstarted() {
  local mark="$OOM_SCOPE_MARK"
  OOM_SCOPE_MARK=""
  [[ -n "$mark" && -e "$mark" ]] || return 1
  rm -f -- "$mark"
  return 0
}

# oom_run_scoped <name> <preference> <cmd>...: run cmd in the foreground in
# its own scope, or bare when the scope cannot be had or fails to start.
# Returns cmd's exit status.
oom_run_scoped() {
  local name="$1" pref="$2" rc
  shift 2
  oom_scope_prefix "$name" "$pref"
  if (( ${#OOM_SCOPE_PREFIX[@]} == 0 )); then
    "$@"; return
  fi
  "${OOM_SCOPE_PREFIX[@]}" "$@"
  rc=$?
  if oom_scope_unstarted; then
    echo "oom-policy: the $name scope failed to start, running it in the caller's cgroup" >&2
    "$@"; return
  fi
  return "$rc"
}

# oom_supervise <name> <preference|off> <cmd>...: run cmd in the background
# with the caller's stdin, restart it after an OOM death (exit 137 = SIGKILL,
# 3 = -XX:+ExitOnOutOfMemoryError), and return its status on any other exit.
# Every launch gets a fresh scope (see oom_scope_prefix); a scope that fails
# to start falls back to a bare launch instead of ending the supervisor.
# Knobs: OOM_MAX_RESTARTS (5) within OOM_RESTART_WINDOW seconds (600),
# OOM_RESTART_DELAY seconds between restarts (5).
_oom_child=""
_oom_launch() {
  local c=0
  ${OOM_SCOPE_PREFIX[@]+"${OOM_SCOPE_PREFIX[@]}"} "$@" <&0 &
  _oom_child=$!
  wait "$_oom_child" || c=$?
  _oom_child=""
  return "$c"
}
oom_supervise() {
  local name="$1" pref="$2" code now t
  shift 2
  local max="${OOM_MAX_RESTARTS:-5}" window="${OOM_RESTART_WINDOW:-600}" delay="${OOM_RESTART_DELAY:-5}"
  local restarts=() recent=()
  trap '[[ -n "$_oom_child" ]] && kill -TERM "$_oom_child" 2>/dev/null; wait "$_oom_child" 2>/dev/null; exit 143' INT TERM
  while true; do
    OOM_SCOPE_PREFIX=()
    OOM_SCOPE_MARK=""
    [[ "$pref" != "off" ]] && oom_scope_prefix "$name" "$pref"
    code=0
    _oom_launch "$@" || code=$?
    if oom_scope_unstarted; then
      echo "oom-policy: the $name scope failed to start (exit $code), launching it in the caller's cgroup" >&2
      OOM_SCOPE_PREFIX=()
      code=0
      _oom_launch "$@" || code=$?
    fi
    if [[ "$code" != 137 && "$code" != 3 ]]; then
      return "$code"
    fi
    now=$SECONDS
    recent=()
    for t in ${restarts[@]+"${restarts[@]}"}; do
      (( now - t < window )) && recent+=("$t")
    done
    restarts=(${recent[@]+"${recent[@]}"} "$now")
    if (( ${#restarts[@]} > max )); then
      echo "$name: exit $code, $max restarts within ${window}s, giving up" >&2
      return "$code"
    fi
    echo "$name: exit $code (OOM) at $(date -Is), restart ${#restarts[@]}/$max in ${delay}s" >&2
    sleep "$delay"
  done
}

# oom_check <pid>: print the OOM standing of one process. Exit 1 if absent.
oom_check() {
  local pid="${1:?usage: oom_check <pid>}" cg path xattr=""
  [[ -r "/proc/$pid/oom_score_adj" ]] || { echo "pid $pid: not running" >&2; return 1; }
  cg="$(awk -F: '$1=="0"{print $3}' "/proc/$pid/cgroup" 2>/dev/null)"
  path="/sys/fs/cgroup${cg}"
  if command -v python3 >/dev/null 2>&1 && [[ -d "$path" ]]; then
    xattr="$(python3 -c 'import os,sys
p=sys.argv[1]
print(" ".join(a+"="+os.getxattr(p,a).decode() for a in os.listxattr(p) if "oomd" in a) or "none")' "$path" 2>/dev/null)"
  fi
  printf 'pid=%s oom_score_adj=%s oom_score=%s cgroup=%s oomd=%s\n' \
    "$pid" "$(cat "/proc/$pid/oom_score_adj")" "$(cat "/proc/$pid/oom_score")" \
    "${cg:-?}" "${xattr:-unknown}"
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
  case "${1:-}" in
    check) shift; for p in "$@"; do oom_check "$p" || exit 1; done ;;
    *) echo "usage: $0 check <pid>..." >&2; exit 2 ;;
  esac
fi
