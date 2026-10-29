#!/usr/bin/env bash
# bin/oom-policy-test.sh — proves bin/oom-policy.sh against the live kernel.
# Every case reads /proc/<pid>/oom_score_adj (and the cgroup xattr systemd-oomd
# reads) of a real child process. No JVM, no network; runs in a few seconds.
#
#   bin/oom-policy-test.sh            # prints one line per case, exits 1 on failure
#
# Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
# SPDX-License-Identifier: AGPL-3.0-or-later
# Cases change PATH inside $(...) on purpose (fake systemd-run, local to the
# case) and pass sh -c bodies whose $1 the inner sh expands.
# shellcheck disable=SC2030,SC2031,SC2016
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
scoped=""
if (( ${#OOM_SCOPE_PREFIX[@]} == 0 )); then
  echo "skip oomd scope: no systemd user manager (scope-only cases below are skipped)"
else
  scoped=1
  # shellcheck disable=SC2016  # expanded by the inner bash, not here
  line="$(OOM_POLICY="$SCRIPT_DIR/oom-policy.sh" timeout 20 "${OOM_SCOPE_PREFIX[@]}" \
            bash -c 'source "$OOM_POLICY"; oom_check "$BASHPID"')"
  case "$line" in
    *"/app.slice/oom-policy-test-"*"user.oomd_avoid=1"*) ok "oomd: own scope with user.oomd_avoid=1 ($line)" ;;
    *) bad "oomd: scope/xattr missing: $line" ;;
  esac
  # systemd-run --scope execs in place: the caller's adj survives into the scope.
  oom_scope_prefix oom-policy-test avoid 2>/dev/null
  got="$( (oom_adj_set 800 2>/dev/null; timeout 20 "${OOM_SCOPE_PREFIX[@]}" sh -c 'cat /proc/self/oom_score_adj') )"
  expect "oomd: adj survives the scope" "$got" "800"
  oom_scope_unstarted && bad "oomd: marker survived a started scope"
fi

# 5. oom_check on a missing pid fails loudly.
if oom_check 0 2>/dev/null; then bad "oom_check 0 should fail"; else ok "oom_check: missing pid -> rc 1"; fi

TMP="$(mktemp -d "${TMPDIR:-/tmp}/oom-policy-test.XXXXXXXX")"
orphans="$TMP/orphans"
# shellcheck disable=SC2317,SC2329  # reached via trap
cleanup() {
  [[ -f "$orphans" ]] && xargs -r kill 2>/dev/null < "$orphans"
  rm -rf "$TMP"
}
trap cleanup EXIT

# Fake systemd-run binaries for the failure cases. "probe-ok" answers the
# `... true` probe and fails every real launch (manager gone in between);
# "dead" fails everything (no user manager).
mkdir -p "$TMP/probe-ok" "$TMP/dead"
cat > "$TMP/probe-ok/systemd-run" <<'EOF'
#!/bin/sh
for last; do :; done
[ "$last" = true ] && exit 0
echo "Failed to start transient scope unit: fake" >&2
exit 1
EOF
printf '#!/bin/sh\necho "Failed to connect to bus: fake" >&2\nexit 1\n' > "$TMP/dead/systemd-run"
chmod +x "$TMP/probe-ok/systemd-run" "$TMP/dead/systemd-run"

# 6. An invalid preference is refused before systemd sees it: no prefix,
#    a message, rc 0.
msg="$(oom_scope_prefix x bogus 2>&1)"; rc=$?
oom_scope_prefix x bogus 2>/dev/null
expect "invalid pref: rc 0 and no prefix" "rc=$rc n=${#OOM_SCOPE_PREFIX[@]}" "rc=0 n=0"
case "$msg" in *"not avoid|omit|none"*) ok "invalid pref: says why" ;; *) bad "invalid pref: message [$msg]" ;; esac

# 7. The probe is a real scope start: a systemd-run that cannot start one
#    yields no prefix (the old show-environment probe passed here).
got="$( PATH="$TMP/dead:$PATH"; oom_scope_prefix x none 2>/dev/null; echo "n=${#OOM_SCOPE_PREFIX[@]}" )"
expect "probe: failing systemd-run -> no prefix" "$got" "n=0"

# 8. A scope that fails to START falls back to a bare launch, exactly once;
#    a command that ran and failed is NOT re-run.
runs="$TMP/runs8"
got="$( PATH="$TMP/probe-ok:$PATH"
        oom_run_scoped x none sh -c 'echo run >> "$1"; exit 7' sh "$runs" 2>/dev/null
        echo "rc=$? runs=$(wc -l < "$runs")" )"
expect "fallback: unstarted scope -> one bare run, its rc kept" "$got" "rc=7 runs=1"
if [[ -n "$scoped" ]]; then
  runs="$TMP/runs8b"
  oom_run_scoped x none sh -c 'echo run >> "$1"; exit 7' sh "$runs" 2>/dev/null; rc=$?
  expect "fallback: a started scope whose command fails is not re-run" "rc=$rc runs=$(wc -l < "$runs")" "rc=7 runs=1"
fi

# 9. The supervisor survives a scope that fails to start: it launches bare
#    instead of exiting 1 (the old loop's restartable_exit rejected 1).
runs="$TMP/runs9"
got="$( PATH="$TMP/probe-ok:$PATH" OOM_RESTART_DELAY=0
        oom_supervise x none sh -c 'echo run >> "$1"; exit 0' sh "$runs" 2>/dev/null </dev/null
        echo "rc=$? runs=$(wc -l < "$runs")" )"
expect "supervise: unstarted scope -> bare launch, rc 0" "$got" "rc=0 runs=1"

# 10. Restart with the previous scope still loaded: the first launch leaves an
#     orphan in its scope and dies with 137; the restart must get a NEW scope
#     (a reused --unit name fails "already loaded") and run inside it.
if [[ -n "$scoped" ]]; then
  stub="$TMP/stub10"
  # shellcheck disable=SC2016  # expanded by the stub
  printf '%s\n' '#!/bin/bash' \
    'n=$(( $(cat "$1/n" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$1/n"' \
    'awk -F: '\''$1=="0"{print $3}'\'' /proc/$$/cgroup >> "$1/cgroups"' \
    'if [ "$n" = 1 ]; then timeout 60 sleep 60 & echo $! >> "$1/orphans_pid"; exit 137; fi' \
    'exit 0' > "$stub"
  chmod +x "$stub"
  OOM_RESTART_DELAY=0 oom_supervise oom-policy-test none "$stub" "$TMP" 2>"$TMP/err10" </dev/null; rc=$?
  cat "$TMP/orphans_pid" >> "$orphans" 2>/dev/null
  mapfile -t cgs < "$TMP/cgroups"
  expect "restart: second launch ran and supervisor returned its 0" "rc=$rc launches=${#cgs[@]}" "rc=0 launches=2"
  if [[ ${#cgs[@]} == 2 && "${cgs[0]}" != "${cgs[1]}" && "${cgs[1]}" == *"/oom-policy-test-"* ]]; then
    ok "restart: fresh scope per launch (${cgs[0]##*/} -> ${cgs[1]##*/})"
  else
    bad "restart: scopes [${cgs[*]}] $(cat "$TMP/err10")"
  fi
  # The first scope really is still loaded (orphan alive), which is what made
  # the old fixed --unit name fail.
  if [[ -n "${cgs[0]:-}" ]] && [[ -d "/sys/fs/cgroup${cgs[0]}" ]]; then
    ok "restart: the first scope was still loaded during the restart"
  else
    echo "note restart: first scope already gone; reuse case not exercised"
  fi
fi

# 11. bin/test-sandboxed.sh moves its JVM (a stub `clojure` here, no JVM) into
#     its own sibling scope: a different cgroup from the caller, adj 800.
mkdir -p "$TMP/bin"
# shellcheck disable=SC2016  # expanded by the stub
printf '%s\n' '#!/bin/sh' \
  'awk -F: '\''$1=="0"{print $3}'\'' /proc/$$/cgroup > "$STUB_OUT"' \
  'cat /proc/$$/oom_score_adj >> "$STUB_OUT"' > "$TMP/bin/clojure"
chmod +x "$TMP/bin/clojure"
self_cg="$(awk -F: '$1=="0"{print $3}' /proc/$BASHPID/cgroup)"
STUB_OUT="$TMP/sbx" PATH="$TMP/bin:$PATH" timeout 60 "$SCRIPT_DIR/test-sandboxed.sh" --focus none-such >/dev/null 2>"$TMP/sbx.err"; rc=$?
mapfile -t sbx < "$TMP/sbx" 2>/dev/null
expect "sandbox: stub ran, rc 0, adj 800" "rc=$rc adj=${sbx[1]:-}" "rc=0 adj=800"
if [[ -n "$scoped" ]]; then
  if [[ -n "${sbx[0]:-}" && "${sbx[0]}" != "$self_cg" && "${sbx[0]}" == *"/hive-mcp-test-"* ]]; then
    ok "sandbox: own scope ${sbx[0]##*/}, caller in ${self_cg##*/}"
  else
    bad "sandbox: cgroup [${sbx[0]:-}] vs caller [$self_cg] $(tail -3 "$TMP/sbx.err")"
  fi
fi
# ... and with systemd-run failing to start the scope it still runs, once, bare.
STUB_OUT="$TMP/sbx2" PATH="$TMP/bin:$TMP/probe-ok:$PATH" timeout 60 "$SCRIPT_DIR/test-sandboxed.sh" --focus none-such >/dev/null 2>"$TMP/sbx2.err"; rc=$?
mapfile -t sbx < "$TMP/sbx2" 2>/dev/null
expect "sandbox fallback: bare run in the caller's cgroup" "rc=$rc cg=${sbx[0]:-}" "rc=0 cg=$self_cg"

# bin/clj-test: the `clojure -M:test*` wrapper. A fake clojure records the adj
# it was started with and its argv; the real clojure is never run.
printf '#!/bin/sh\necho "adj=$(cat /proc/$$/oom_score_adj) args=$*"\n' > "$TMP/fake-clojure"
chmod +x "$TMP/fake-clojure"
got="$(HIVE_CLJ_TEST_CLOJURE="$TMP/fake-clojure" HIVE_TEST_OOMD_PREFERENCE=off \
        timeout 20 "$SCRIPT_DIR/clj-test" -M:test-unit -n a.b-test 2>/dev/null)"
expect "clj-test: child raised to 800, argv untouched" "$got" "adj=800 args=-M:test-unit -n a.b-test"
got="$(HIVE_CLJ_TEST_CLOJURE="$TMP/fake-clojure" HIVE_TEST_OOM_SCORE_ADJ=500 \
        timeout 20 "$SCRIPT_DIR/clj-test" -M:test 2>/dev/null)"
expect "clj-test: HIVE_TEST_OOM_SCORE_ADJ=500 (scoped launch)" "$got" "adj=500 args=-M:test"

expect "this shell untouched" "$(cat /proc/self/oom_score_adj)" "$base"
echo "oom-policy-test: $pass passed, $fail failed"
(( fail == 0 ))
