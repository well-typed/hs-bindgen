#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

# All subsequent relative paths (`-I hs-project/direct-optimization`, the
# header argument, and `hs-project/`) resolve from the example directory, so
# make that the cwd unconditionally.
cd "$SCRIPT_DIR"

echo "# "
echo "# Generating Haskell bindings"
echo "# "

# The C library is vendored under hs-project/direct-optimization and compiled
# by cabal via `c-sources`, so there is nothing to build or install first; see
# bundled-c for the same approach.
cabal run --project-file="${PROJECT_ROOT}/cabal.project" hs-bindgen-cli -- \
    preprocess \
    -I hs-project/direct-optimization \
    --hs-output-dir hs-project/generated \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.amnh.directoptimization \
    --module DirectOptimization \
    --select-by-decl-name '^(cAlign2D|setUp2dCostMtx|setUp3dCostMtx|freeCostMtx)$' \
    --enable-program-slicing \
    --prescriptive-binding-spec binding-specs/direct-optimization.yaml \
    --single-file \
    --unsafe '' \
    c_alignment_interface.h

echo "# "
echo "# Done!"
echo "# "

(
    echo "# "
    echo "Running the project"
    echo "# "

    cd "$SCRIPT_DIR/hs-project"
    cabal build c-direct-optimization-bin

    # The executable aligns two IUPAC DNA sequences given as arguments, so a
    # sample pair is supplied here to make the example self-running.
    cabal run c-direct-optimization-bin -- ACGTRYKM ACTTRYK
)
