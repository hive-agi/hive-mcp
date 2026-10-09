#!/usr/bin/env bash
# ============================================================================
# bin/test-count-floor.sh — judge a cognitect.test-runner log by its COUNT
# ----------------------------------------------------------------------------
# cognitect.test-runner exits 1 both for ordinary test failures and for a
# namespace that fails to LOAD. A load abort can drop dozens of namespaces
# from the run while the printed summary still looks like a normal result, so
# the exit code alone cannot tell "N tests ran, some red" from "most of the
# suite never ran" (kanban 20260729123146-335b876e, principle
# 20260712010941-19acb93b: check the test COUNT, not the exit code).
#
# This script reads a saved runner log and the runner's exit code and decides:
#
#   exit 0  the summary line is present, failures+errors are 0, and the run
#           met the committed floor of tests and assertions
#   exit 2  LOAD ABORT: a compile/load error is in the log, or there is no
#           "Ran N tests" summary at all
#   exit 3  COUNT BELOW FLOOR: fewer tests or assertions than committed
#   exit 1  ordinary test failures/errors (or the runner exited non-zero
#           with a clean summary)
#
# A green runner exit with 0 tests, or with a shrunken suite, is therefore
# impossible to report as success.
#
# Usage:
#   clojure -M:test-unit 2>&1 | tee test.log; rc=${PIPESTATUS[0]}
#   bin/test-count-floor.sh test.log "$rc"
#
# The floor lives in the two shell vars below on purpose: raising it is a
# one-line reviewed change. Raise it when the suite grows;
# lowering it needs a reason in the commit message.
# ============================================================================
set -euo pipefail

# Floor measured on GitHub Actions CI run 37863003751 (staging bf593e10,
# 2026-10-09): "Ran 6425 tests containing 26361 assertions." The floor sits
# slightly below that so deleting a handful of dead tests does not need a
# floor change, while losing a whole namespace chunk does trip it.
MIN_TESTS="${MIN_TESTS:-6300}"
MIN_ASSERTIONS="${MIN_ASSERTIONS:-25800}"

log="${1:?usage: test-count-floor.sh <runner-log> [runner-exit-code]}"
rc="${2:-0}"

[ -r "$log" ] || { echo "test-count-floor: cannot read $log" >&2; exit 2; }

# A namespace that does not compile is reported by the runner as an
# exception from load/require before (or instead of) the summary.
if grep -Eq 'Syntax error (compiling|macroexpanding)|CompilerException|Could not locate .* on classpath|Execution error .* at clojure\.lang\.Compiler' "$log"; then
  echo "test-count-floor: LOAD ABORT — a namespace failed to compile or load:" >&2
  grep -En 'Syntax error|CompilerException|Could not locate' "$log" | head -10 >&2
  exit 2
fi

summary="$(grep -E '^Ran [0-9]+ tests containing [0-9]+ assertions\.' "$log" | tail -1 || true)"
if [ -z "$summary" ]; then
  echo "test-count-floor: LOAD ABORT — no 'Ran N tests containing M assertions.' line (runner exit $rc)" >&2
  exit 2
fi

tests="$(echo "$summary" | awk '{print $2}')"
assertions="$(echo "$summary" | awk '{print $5}')"
result="$(grep -E '^[0-9]+ failures, [0-9]+ errors\.' "$log" | tail -1 || true)"
failures="$(echo "$result" | awk '{print $1}')"
errors="$(echo "$result" | awk '{print $3}')"

echo "test-count-floor: ran $tests tests / $assertions assertions (floor $MIN_TESTS / $MIN_ASSERTIONS); ${failures:-?} failures, ${errors:-?} errors; runner exit $rc"

if [ "$tests" -lt "$MIN_TESTS" ] || [ "$assertions" -lt "$MIN_ASSERTIONS" ]; then
  echo "test-count-floor: COUNT BELOW FLOOR — part of the suite did not run" >&2
  exit 3
fi

if [ "${failures:-1}" != "0" ] || [ "${errors:-1}" != "0" ] || [ "$rc" != "0" ]; then
  exit 1
fi

exit 0
