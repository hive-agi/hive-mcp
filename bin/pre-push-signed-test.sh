#!/usr/bin/env bash
# bin/pre-push-signed-test.sh — proves bin/pre-push-signed.sh on a throwaway
# repository: SSH-signed commits pass, an unsigned or @localhost commit in the
# pushed range is refused, a branch deletion and an already-pushed range pass.
# No JVM, no network, no keyring of the caller (a fresh SSH key per run, and
# the caller's global git config is not read).
#
#   bin/pre-push-signed-test.sh      # one line per case, exits 1 on failure
set -uo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOOK="$SCRIPT_DIR/pre-push-signed.sh"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 HOME="$tmp"
zero='0000000000000000000000000000000000000000'

ssh-keygen -q -t ed25519 -N '' -C fixture -f "$tmp/key" >/dev/null
echo "dev@example.org $(cat "$tmp/key.pub")" > "$tmp/allowed"

git init -q -b main "$tmp/remote.git" --bare
git init -q -b main "$tmp/w"
cd "$tmp/w" || exit 2
git config user.name Dev
git config user.email dev@example.org
git config gpg.format ssh
git config user.signingkey "$tmp/key"
git config gpg.ssh.allowedSignersFile "$tmp/allowed"
git remote add origin "$tmp/remote.git"

signed()   { echo "$1" >> f; git add f; git commit -q -S -m "$1"; git rev-parse HEAD; }
unsigned() { echo "$1" >> f; git add f; git -c commit.gpgsign=false commit -q -m "$1"; git rev-parse HEAD; }

base=$(signed base)
git push -q origin main 2>/dev/null

pass=0; fail=0
# case <name> <want-exit> <stdin line>
case_() {
  local name="$1" want="$2" input="$3"
  printf '%s\n' "$input" | "$HOOK" origin >/dev/null 2>&1
  local got=$?
  if [[ "$got" == "$want" ]]; then pass=$((pass + 1)); echo "ok   $name (exit $got)"
  else fail=$((fail + 1)); echo "FAIL $name: want exit $want got $got"; fi
}

s1=$(signed one)
case_ "signed commits pass"                    0 "refs/heads/main $s1 refs/heads/main $base"
u1=$(unsigned two)
s2=$(signed three)
case_ "an unsigned commit inside the range"    1 "refs/heads/main $s2 refs/heads/main $base"
case_ "a new branch carrying it is refused"    1 "refs/heads/feat $s2 refs/heads/feat $zero"
case_ "the range already on the remote passes" 0 "refs/heads/main $s1 refs/heads/main $s1"
case_ "a branch deletion passes"               0 "(delete) $zero refs/heads/old $base"
case_ "empty stdin passes"                     0 ""

git reset -q --hard "$s1"
git config user.email klein@localhost
echo "klein@localhost $(cat "$tmp/key.pub")" >> "$tmp/allowed"
l1=$(signed four)
case_ "signed but @localhost is refused"       1 "refs/heads/main $l1 refs/heads/main $base"

echo "pre-push-signed-test: $pass passed, $fail failed"
[[ "$fail" == 0 ]]
