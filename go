#!/usr/bin/env sh

set -e

export SHOW_DRAFTS=True

# ./scripts/git-metadata

stack run --nix -- clean && stack run --nix -- watch
