#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

(
    echo "# "
    echo "# Building minisat C bindings"
    echo "# "

    git submodule init minisat-c-bindings
    git submodule update minisat-c-bindings

    cd "$SCRIPT_DIR/minisat-c-bindings"
    make
)

(
    echo "# "
    echo "# Creating symlinks for shared library"
    echo "# "

    cd "$SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib"
    ln -sf libminisat-c.so.1.0.0 libminisat-c.so
    ln -sf libminisat-c.so.1.0.0 libminisat-c.so.1
)

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I "minisat-c-bindings" \
    --hs-output-dir "hs-project/src" \
    --module Generated.Minisat \
    "minisat.h"

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(cat <<-EOF
package c-minisat
    extra-include-dirs:
        $SCRIPT_DIR/minisat-c-bindings
      , $SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib
    extra-lib-dirs:
        $SCRIPT_DIR/minisat-c-bindings
      , $SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib
EOF
)
grep -qxF "$LINE" "$SCRIPT_DIR/hs-project/cabal.project.local" || echo "$LINE" >> "$SCRIPT_DIR/hs-project/cabal.project.local"
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "

(
    echo "# "
    echo "Running the project"
    echo "# "

    cd "$SCRIPT_DIR/hs-project"
    LD_LIBRARY_PATH="$SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib:$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH
    cabal build c-minisat-bin
    cabal run c-minisat-bin
)
