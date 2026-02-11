#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I hs-project/cbits \
    --unique-id bundled-c.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Rect \
    --create-output-dirs \
    --overwrite-files \
    rect.h

echo "# "
echo "# Building and running the project"
echo "# "

(
    cd "hs-project"
    cabal build
    cabal run
)
