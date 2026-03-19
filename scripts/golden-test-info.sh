#!/usr/bin/env bash

# Get info about a golden test on the command line. Useful for debugging and
# designing tests.

set -e

usage() {
    echo "Usage: $(basename "$0") COMMAND TEST [OTHER_ARGUMENTS]"
    echo
    echo "For example,"
    echo "    $(basename "$0") use-decl-graph program-analysis/selection_omit_prescriptive.h"
    echo
    echo "OTHER_ARGUMENTS are passed on to 'hs-bindgen-cli info COMMAND'."
}

if [ $# -lt 2 ]; then
    usage
    exit 1
fi

TMP=$(mktemp -d)
echo "Temporary output directory: ${TMP}"

COMMAND="$1"
shift

cabal run -- \
    hs-bindgen-cli info "$COMMAND" \
    --unique-id "$(basename "${TMP}")" \
    -I ./hs-bindgen/examples/golden \
    -I ./hs-bindgen/musl-include/x86_64 \
    "$@"
