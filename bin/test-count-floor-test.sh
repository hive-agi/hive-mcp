#!/usr/bin/env bash
# bin/test-count-floor-test.sh — proves bin/test-count-floor.sh on synthetic
# runner logs: one green log and one log per failure mode the gate must catch.
# No JVM; runs in well under a second.
#
#   bin/test-count-floor-test.sh      # one line per case, exits 1 on failure
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GATE="$SCRIPT_DIR/test-count-floor.sh"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

pass=0; fail=0
# case <name> <want-exit> <runner-rc> <log-body> [env assignments...]
case_() {
  local name="$1" want="$2" rc="$3" body="$4"; shift 4
  printf '%s\n' "$body" > "$tmp/log"
  env "$@" "$GATE" "$tmp/log" "$rc" >/dev/null 2>&1
  local got=$?
  if [[ "$got" == "$want" ]]; then pass=$((pass + 1)); echo "ok   $name (exit $got)"
  else fail=$((fail + 1)); echo "FAIL $name: want exit $want got $got"; fi
}

green=$'Running tests in #{"test"}\n\nTesting hive-mcp.foo-test\n\nRan 13805 tests containing 53607 assertions.\n0 failures, 0 errors.'

case_ "green log passes"                       0 0 "$green"
case_ "no summary (crash before Ran line)"     2 1 $'Running tests in #{"test"}\nException in thread "main" java.lang.OutOfMemoryError'
case_ "empty log"                              2 0 ""
case_ "namespace load abort"                   2 1 $'Syntax error compiling at (hive_mcp/foo_test.clj:1:1).\nRan 13805 tests containing 53607 assertions.\n0 failures, 0 errors.'
case_ "zero tests ran"                         3 0 $'Ran 0 tests containing 0 assertions.\n0 failures, 0 errors.'
case_ "zero tests ran even with floor at 0"    3 0 $'Ran 0 tests containing 0 assertions.\n0 failures, 0 errors.' MIN_TESTS=0 MIN_ASSERTIONS=0
case_ "suite shrank below floor"               3 0 $'Ran 900 tests containing 4000 assertions.\n0 failures, 0 errors.'
case_ "red tests"                              1 1 $'Ran 13805 tests containing 53607 assertions.\n2 failures, 0 errors.'
case_ "errors"                                 1 1 $'Ran 13805 tests containing 53607 assertions.\n0 failures, 1 errors.'
case_ "clean summary but runner exited 1"      1 1 "$green"
case_ "summary without result line"            1 0 'Ran 13805 tests containing 53607 assertions.'
"$GATE" "$tmp/does-not-exist" 0 >/dev/null 2>&1; got=$?
if [[ "$got" == 2 ]]; then pass=$((pass + 1)); echo "ok   unreadable log path (exit 2)"; else fail=$((fail + 1)); echo "FAIL unreadable log path: got $got"; fi

echo "test-count-floor-test: $pass passed, $fail failed"
[[ "$fail" == 0 ]]
