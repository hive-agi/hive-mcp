#!/usr/bin/env bash
# bin/pre-push-signed.sh — git pre-push hook: refuse to push a commit that
# main's ruleset ("Commits must have verified signatures") will later refuse.
#
# A promotion PR that carries one unsigned commit is green on every check and
# still BLOCKED, and the repair is a rewrite of every commit after it. This
# hook moves that refusal to the push of the offending commit, where the fix
# is one amend.
#
# Install (per clone):   ln -sf ../../bin/pre-push-signed.sh .git/hooks/pre-push
# or, with a hooks dir:  ln -sf "$PWD/bin/pre-push-signed.sh" "$(git rev-parse --git-path hooks)/pre-push"
#
# git feeds one line per ref on stdin: <local-ref> <local-sha> <remote-ref> <remote-sha>.
# Every commit being pushed (local-sha not yet on the remote) is checked:
#   %G? N (no signature) or B (bad signature)       -> refused
#   author or committer email ending in @localhost  -> refused (GitHub cannot
#        attribute it, so a valid signature still reads `no_user` there)
#   %G? E/X/Y/R/U (signed, but not verifiable here)  -> allowed with a warning
# Exit 0 when every commit passes, 1 when any is refused.
set -uo pipefail

remote="${1:-origin}"
zero='0000000000000000000000000000000000000000'
refused=0
warned=0

check_range() {
  # $@: rev-list arguments naming the commits being pushed
  local sig sha ae ce subj
  local -a shas
  mapfile -t shas < <(git rev-list "$@" 2>/dev/null)
  (( ${#shas[@]} == 0 )) && return 0
  while IFS=$'\t' read -r sig sha ae ce subj; do
    [[ -z "$sha" ]] && continue
    case "$sig" in
      N|B)
        echo "pre-push-signed: REFUSED ${sha:0:12} (signature $sig): $subj" >&2
        refused=$((refused + 1)) ;;
      G) ;;
      *)
        echo "pre-push-signed: warning ${sha:0:12} signature $sig (signed, not verifiable on this host): $subj" >&2
        warned=$((warned + 1)) ;;
    esac
    case "$ae $ce" in
      *@localhost\ *|*@localhost)
        echo "pre-push-signed: REFUSED ${sha:0:12} unattributed identity ($ae / $ce): $subj" >&2
        refused=$((refused + 1)) ;;
    esac
  done < <(git log --no-walk=unsorted --format=$'%G?\t%H\t%ae\t%ce\t%s' \
             "${shas[@]}" 2>/dev/null)
}

while read -r local_ref local_sha remote_ref remote_sha; do
  [[ -z "${local_sha:-}" ]] && continue
  # a branch deletion pushes no commits
  [[ "$local_sha" == "$zero" ]] && continue
  if [[ "$remote_sha" == "$zero" ]]; then
    # a new remote branch: everything the remote does not already have
    check_range "$local_sha" --not --remotes="$remote"
  else
    check_range "$remote_sha..$local_sha"
  fi
done

if (( refused > 0 )); then
  cat >&2 <<EOF
pre-push-signed: $refused problem(s); nothing was pushed.
  Fix the newest offender with: git commit --amend -S --reset-author --no-edit
  (older ones: git rebase -i --exec 'git commit --amend -S --no-edit --reset-author' <base>).
  Check identity: git config user.email ; git config commit.gpgsign
EOF
  exit 1
fi
(( warned > 0 )) && echo "pre-push-signed: $warned commit(s) signed but not verifiable here; GitHub decides." >&2
exit 0
