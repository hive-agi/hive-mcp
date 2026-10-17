#!/usr/bin/env bash
# bin/oom-policy-test.sh — proves bin/oom-policy.sh against the live kernel.
# Every case reads /proc/<pid>/oom_score_adj (and the cgroup xattr systemd-oomd
# reads) of a real child process. No JVM, no network; runs in a few seconds.
#
#   bin/oom-policy-test.sh            # prints one line per case, exits 1 on failure
#
# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=bin/oom-policy.sh
source "$SCRIPT_DIR/oom-policy.sh"

pass=0; fail=0
ok()  { pass=$((pass + 1)); echo "ok   $*"; }
bad() { fail=$((fail + 1)); echo "FAIL $*"; }
expect() { if [[ "$2" == "$3" ]]; then ok "$1"; else bad "$1: want [$3] got [$2]"; fi; }

base="$(cat /proc/self/oom_score_adj)"

# 1. Raising works unprivileged and the child (the JVM's stand-in) inherits it.
got="$( (oom_adj_set 800 2>/dev/null; sh -c 'cat /proc/self/oom_score_adj') )"
expect "test side: raised adj is inherited by the child" "$got" "800"

# 2. Lowering BELOW the value the process started with (the kernel's
#    oom_score_adj_min) is refused without CAP_SYS_RESOURCE; oom_adj_set
#    returns 0 and leaves the adj unchanged, so a launcher never aborts on it.
#    (Lowering back down to that floor, e.g. 700 -> 600 from 0, is allowed.)
low=$((base - 100))
got="$( (oom_adj_set 700 2>/dev/null; oom_adj_set "$low" 2>/dev/null; echo "rc=$? adj=$(cat /proc/self/oom_score_adj)") )"
if [[ "$got" == "rc=0 adj=$low" ]]; then
  ok "server side: lowering to $low succeeds (CAP_SYS_RESOURCE present)"
else
  expect "server side: unprivileged lowering below $base is a no-op" "$got" "rc=0 adj=700"
fi

# 3. A test JVM stand-in outranks a server stand-in in the kernel's eyes.
srv="$( (oom_adj_set -500 2>/dev/null; sh -c 'cat /proc/self/oom_score_adj') )"
tst="$( (oom_adj_set 800 2>/dev/null; sh -c 'cat /proc/self/oom_score_adj') )"
if (( tst > srv )); then ok "ordering: test adj $tst > server adj $srv"; else bad "ordering: test $tst <= server $srv"; fi

# 4. The systemd-oomd lever: the scope carries ManagedOOMPreference=avoid,
#    which systemd writes as the cgroup xattr user.oomd_avoid=1.
oom_scope_prefix oom-policy-test avoid 2>/dev/null
if (( ${#OOM_SCOPE_PREFIX[@]} == 0 )); then
  echo "skip oomd scope: no systemd user manager"
else
  # shellcheck disable=SC2016  # expanded by the inner bash, not here
  line="$(OOM_POLICY="$SCRIPT_DIR/oom-policy.sh" timeout 20 "${OOM_SCOPE_PREFIX[@]}" \
            bash -c 'source "$OOM_POLICY"; oom_check "$BASHPID"')"
  case "$line" in
    *"/app.slice/oom-policy-test-"*"user.oomd_avoid=1"*) ok "oomd: own scope with user.oomd_avoid=1 ($line)" ;;
    *) bad "oomd: scope/xattr missing: $line" ;;
  esac
  # systemd-run --scope execs in place: the caller's adj survives into the scope.
  got="$( (oom_adj_set 800 2>/dev/null; timeout 20 "${OOM_SCOPE_PREFIX[@]}" sh -c 'cat /proc/self/oom_score_adj') )"
  expect "oomd: adj survives the scope" "$got" "800"
fi

# 5. oom_check on a missing pid fails loudly.
if oom_check 0 2>/dev/null; then bad "oom_check 0 should fail"; else ok "oom_check: missing pid -> rc 1"; fi

expect "this shell untouched" "$(cat /proc/self/oom_score_adj)" "$base"
echo "oom-policy-test: $pass passed, $fail failed"
(( fail == 0 ))
