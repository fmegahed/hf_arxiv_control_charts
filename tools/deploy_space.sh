#!/usr/bin/env bash
# Deploy the current commit to the Hugging Face Space.
#
# The Space refuses plain files over 10 MB, and the factsheet files grow every
# day. GitHub keeps them as plain files (so raw links and diffs keep working),
# and this script sends the Space one fresh commit in which the factsheets are
# stored with Git LFS. The Space is a deploy target: its history is replaced
# on every deploy, and nobody should push to it by any other route.
#
# Usage: tools/deploy_space.sh <remote-url-or-name> [branch]
#   tools/deploy_space.sh https://USER:TOKEN@huggingface.co/spaces/OWNER/NAME
set -euo pipefail

remote="${1:?Usage: tools/deploy_space.sh <remote-url-or-name> [branch]}"
branch="${2:-main}"
# Files stored with Git LFS on the Space (data/frozen is not matched by this).
lfs_patterns=("data/*_factsheet.csv")

# Files must reach the Space byte for byte, on Windows too.
export GIT_CONFIG_COUNT=1 GIT_CONFIG_KEY_0=core.autocrlf GIT_CONFIG_VALUE_0=false

source_commit="$(git rev-parse HEAD)"
if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "Uncommitted changes to tracked files; commit them before deploying." >&2
  exit 1
fi

repo="$(git rev-parse --show-toplevel)"
work="$(mktemp -d)"
cleanup() {
  cd "$repo"
  git worktree remove --force "$work" >/dev/null 2>&1 || true
  git branch -D space-deploy >/dev/null 2>&1 || true
}
trap cleanup EXIT

git branch -D space-deploy >/dev/null 2>&1 || true
git worktree add --quiet --detach "$work" "$source_commit"
cd "$work"
git checkout --quiet --orphan space-deploy
# The index still holds every tracked file, including images that .gitignore
# would otherwise leave out. Only the LFS files are added again, so that they
# are stored as LFS pointers.
git lfs track "${lfs_patterns[@]}" >/dev/null
git add .gitattributes
git rm --quiet --cached -f -- "${lfs_patterns[@]}"
git add -- "${lfs_patterns[@]}"
git -c user.name="${GIT_AUTHOR_NAME:-qe-arxiv-watch deploy}" \
    -c user.email="${GIT_AUTHOR_EMAIL:-deploy@users.noreply.github.com}" \
    commit --quiet -m "Deploy ${source_commit}"

echo "Files stored with Git LFS in this deploy:"
git lfs ls-files --size

# Upload the large files first, then the commit that points to them.
git lfs push "$remote" space-deploy
git push --force --no-verify "$remote" "space-deploy:${branch}"
echo "Deployed ${source_commit} to ${branch}."
