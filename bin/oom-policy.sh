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
# systemd-run prefix that runs the command in its own user scope carrying
# ManagedOOMPreference=<preference> (avoid|omit|none). Empty when no systemd
# user manager answers, so "${OOM_SCOPE_PREFIX[@]}" cmd... always runs cmd.
# systemd-run --scope execs the command in place: same pid, same stdio.
# shellcheck disable=SC2034  # read by the scripts that source this file
OOM_SCOPE_PREFIX=()
# shellcheck disable=SC2034
oom_scope_prefix() {
  local name="$1" pref="$2"
  OOM_SCOPE_PREFIX=()
  if command -v systemd-run >/dev/null 2>&1 \
     && timeout 5 systemctl --user show-environment >/dev/null 2>&1; then
    OOM_SCOPE_PREFIX=(systemd-run --user --scope --collect --quiet
                      --unit "${name}-$$-${RANDOM}"
                      -p "ManagedOOMPreference=${pref}")
  else
    echo "oom-policy: no systemd user manager, $name runs in the caller's cgroup" >&2
  fi
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
