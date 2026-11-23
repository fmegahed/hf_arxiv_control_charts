#!/usr/bin/env bash
# Push the checked-out branch, retrying when the push is refused.
#
# The data workflows commit to a branch that other runs and people also push
# to. A push is refused when someone pushed first, or when GitHub fails for a
# moment: on 2026-10-19 it answered a push with a 500 and that week's digest
# was lost. Before each retry the commit is rebased onto the remote branch.
# The data jobs write different files, so the rebase applies cleanly. If it
# conflicts, the rebase is undone and the push fails, as a plain push would.
#
# Usage: bash tools/push_with_retry.sh [attempts]   (default 3)
# PUSH_RETRY_WAIT_S sets the pause before each retry (default 20).
set -u

attempts="${1:-3}"
wait_s="${PUSH_RETRY_WAIT_S:-20}"

rebase_in_progress() {
  [ -d "$(git rev-parse --git-path rebase-merge)" ] ||
    [ -d "$(git rev-parse --git-path rebase-apply)" ]
}

for attempt in $(seq 1 "$attempts"); do
  if [ "$attempt" -gt 1 ]; then
    echo "Push refused; trying again in ${wait_s} s (attempt $attempt of $attempts)."
    sleep "$wait_s"
    # --autostash: the job may leave tracked files modified that it did not commit.
    if ! git pull --rebase --autostash --quiet; then
      if rebase_in_progress; then
        git rebase --abort
        echo "This run's commit conflicts with the remote branch; it was not pushed." >&2
        exit 1
      fi
      continue  # the remote could not be read; wait and try again
    fi
  fi
  if git push --quiet; then
    exit 0
  fi
done

echo "Push failed after $attempts attempts." >&2
exit 1
