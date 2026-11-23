#!/usr/bin/env bash
# Tests for tools/push_with_retry.sh, run against throwaway local repositories.
#
# Usage (from the app directory): bash tests/push_with_retry_test.sh
# Exits non-zero when a test fails.
set -u

script="$(cd "$(dirname "$0")/.." && pwd)/tools/push_with_retry.sh"
export PUSH_RETRY_WAIT_S=0
# The same line endings on every platform, whatever the global git config says.
export GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0=core.autocrlf GIT_CONFIG_VALUE_0=false
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
failures=0

check() {  # check <description> <command> [args...]
  local what="$1"; shift
  if "$@"; then
    echo "ok   $what"
  else
    echo "FAIL $what"
    failures=$((failures + 1))
  fi
}

# A remote holding a.txt, b.txt and c.txt, a clone for this run ("job") and
# a clone for whoever else writes to the branch ("other").
setup() {
  rm -rf "${work:?}"/*
  git init -q --bare "$work/remote.git"
  git --git-dir="$work/remote.git" symbolic-ref HEAD refs/heads/main
  git init -q "$work/seed"
  (
    cd "$work/seed" &&
      git checkout -q -b main &&
      echo a > a.txt && echo b > b.txt && echo c > c.txt &&
      git add . && git -c user.name=t -c user.email=t@example.com commit -qm seed &&
      git push -q "$work/remote.git" main
  )
  for clone in job other; do
    git clone -q "$work/remote.git" "$work/$clone"
    git -C "$work/$clone" config user.name "$clone"
    git -C "$work/$clone" config user.email "$clone@example.com"
  done
}

commit_in() {  # commit_in <clone> <file>
  echo "$1" >> "$work/$1/$2"
  git -C "$work/$1" add "$2"
  git -C "$work/$1" commit -qm "$1 changes $2"
}

# The remote refuses the first <n> pushes, as GitHub did with a 500.
refuse_pushes() {
  local hook="$work/remote.git/hooks/pre-receive"
  printf '#!/bin/sh\nn=$(cat "%s/count" 2>/dev/null || echo 0)\nn=$((n + 1))\necho "$n" > "%s/count"\nif [ "$n" -le %d ]; then echo "remote: Internal Server Error" >&2; exit 1; fi\n' \
    "$work" "$work" "$1" > "$hook"
  chmod +x "$hook"
}

run_script() {  # run_script [attempts]; exit status saved in $status
  (cd "$work/job" && bash "$script" "$@") > "$work/out.txt" 2>&1
  status=$?
}

remote_subjects() { git --git-dir="$work/remote.git" log --format=%s main; }
newest_on_remote_is() { [ "$(remote_subjects | head -1)" = "$1" ]; }
remote_has() { remote_subjects | grep -qx "$1"; }
succeeded() { [ "$status" -eq 0 ]; }
failed() { [ "$status" -ne 0 ]; }
output_says() { grep -q "$1" "$work/out.txt"; }
pushes_tried() { [ "$(cat "$work/count" 2>/dev/null)" = "$1" ]; }
no_rebase_left_open() {
  local git_dir
  git_dir="$(git -C "$work/job" rev-parse --absolute-git-dir)"
  [ ! -d "$git_dir/rebase-merge" ] && [ ! -d "$git_dir/rebase-apply" ]
}

echo "# nothing in the way"
setup
commit_in job a.txt
run_script
check "pushes the commit" succeeded
check "the commit is on the remote" newest_on_remote_is "job changes a.txt"

echo "# another run pushed first (the daily and weekly jobs overlapping)"
setup
commit_in other b.txt && git -C "$work/other" push -q
commit_in job a.txt
echo "left over" >> "$work/job/c.txt"
run_script
check "pushes after rebasing onto the other run's commit" succeeded
check "this run's commit is newest" newest_on_remote_is "job changes a.txt"
check "the other run's commit is kept" remote_has "other changes b.txt"
check "a modified file the run did not commit is left as it was" grep -qx "left over" "$work/job/c.txt"

echo "# the remote refuses a push once"
setup
refuse_pushes 1
commit_in job a.txt
run_script
check "pushes on the second attempt" succeeded
check "the commit is on the remote" newest_on_remote_is "job changes a.txt"
check "two pushes were tried" pushes_tried 2

echo "# the remote keeps refusing"
setup
refuse_pushes 99
commit_in job a.txt
run_script 3
check "fails" failed
check "stops after the given number of attempts" pushes_tried 3

echo "# the remote cannot be reached at all"
setup
commit_in job a.txt
git -C "$work/job" remote set-url origin "$work/missing.git"
run_script 3
check "fails" failed
check "keeps trying until the attempts run out" output_says "after 3 attempts"

echo "# another run changed the same file"
setup
commit_in other a.txt && git -C "$work/other" push -q
commit_in job a.txt
run_script
check "fails" failed
check "says the commits conflict" output_says "conflicts"
check "the other run's commit stays newest" newest_on_remote_is "other changes a.txt"
check "the rebase is undone" no_rebase_left_open
check "this run's commit is still checked out" [ "$(git -C "$work/job" log -1 --format=%s)" = "job changes a.txt" ]

if [ "$failures" -gt 0 ]; then
  echo "$failures check(s) failed"
  exit 1
fi
echo "all checks passed"
