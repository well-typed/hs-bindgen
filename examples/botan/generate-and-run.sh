#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

(
    echo "# "
    echo "# Building botan"
    echo "# "

    git submodule init botan
    git submodule update botan

    cd "$SCRIPT_DIR/botan"
    ./configure.py --compiler-cache=ccache
    make
)

BOTAN_DIR=$(realpath botan)

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I "botan/build/include/public" \
    --hs-output-dir "hs-project/src" \
    --create-output-dirs \
    --module Generated.Botan \
    "botan/ffi.h"

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(
    cat <<-EOF
package botan
    extra-include-dirs:
        $BOTAN_DIR/build/include/public
    extra-lib-dirs:
        $BOTAN_DIR
EOF
)
grep -qxF "$LINE" "$SCRIPT_DIR/hs-project/cabal.project.local" || echo "$LINE" >>"$SCRIPT_DIR/hs-project/cabal.project.local"
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "

(
    echo "# "
    echo "Running the project"
    echo "# "

    LD_LIBRARY_PATH="$BOTAN_DIR:$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH

    cd "hs-project"
    cabal build
    cabal run botan
)
