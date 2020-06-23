#!/bin/sh

set -e

export FAST_BUILD=True

# ./scripts/git-metadata

stack run -- clean && stack run -- watch
