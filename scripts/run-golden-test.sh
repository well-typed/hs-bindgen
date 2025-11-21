#!/usr/bin/env bash

# Run a golden test on the command line. Useful for debugging and designing
# tests.

set -e

usage() {
    echo "Usage: $(basename "$0") TEST [OTHER_ARGUMENTS]"
    echo
    echo "For example,"
    echo "    $(basename "$0") program-analysis/selection_omit_prescriptive.h"
    echo
    echo "OTHER_ARGUMENTS are passed on to 'hs-bindgen-cli preprocess'."
}

if [ $# -lt 1 ]; then
    usage
    exit 1
fi

TMP=$(mktemp -d)
echo "Temporary output directory: ${TMP}"

cabal run -- \
    hs-bindgen-cli preprocess \
    -I ./hs-bindgen/examples/golden \
    -I ./hs-bindgen/musl-include/x86_64 \
    --module Example \
    --hs-output-dir "${TMP}" \
    "$@"
