#!/usr/bin/env sh

set -e

export SHOW_DRAFTS=True

./scripts/git-metadata
./scripts/make-image-metadata

cabal run between-books -- watch
