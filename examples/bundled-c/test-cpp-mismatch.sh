#!/usr/bin/env bash

# test-cpp-mismatch.sh
#
# Demonstrates CPP flag mismatch: hs-bindgen generates bindings for the
# 4-field rect (without RECT_3D), but cabal compiles the C code with
# -DRECT_3D (5-field rect). The struct layout disagreement causes the C
# functions to read wrong field values, and Main.hs assertions catch the
# mismatch.
#
# Exit 0 = mismatch detected (expected), exit 1 = unexpected success.
#
# See: https://github.com/well-typed/hs-bindgen/issues/893

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

echo "#"
echo "# Step 1: Generate bindings WITHOUT RECT_3D (4-field struct)"
echo "#"

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I hs-project/cbits \
    --unique-id bundled-c.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Rect \
    --create-output-dirs \
    --overwrite-files \
    rect.h

echo "#"
echo "# Step 2: Clean previous build (flag change requires full rebuild)"
echo "#"

(
    cd hs-project
    cabal clean
)

echo "#"
echo "# Step 3: Build WITH -frect-3d (C compiler sees 5-field struct)"
echo "#"

(
    cd hs-project
    cabal build -frect-3d
)

echo "#"
echo "# Step 4: Run and expect failure (struct layout mismatch)"
echo "#"

set +e
(
    cd hs-project
    cabal run -frect-3d
)
EXIT_CODE=$?
set -e

if [ $EXIT_CODE -ne 0 ]; then
    echo "#"
    echo "# SUCCESS: Program failed as expected (exit code $EXIT_CODE)."
    echo "# This demonstrates the CPP flag mismatch described in issue #893."
    echo "#"
    exit 0
else
    echo "#"
    echo "# UNEXPECTED: Program succeeded. The mismatch was not detected."
    echo "#"
    exit 1
fi
