#!/bin/sh

set -e

export SHOW_DRAFTS=True

# ./scripts/git-metadata

stack run -- clean && stack run -- watch
