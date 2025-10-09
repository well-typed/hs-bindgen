#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "# "
echo "# Building minisat C bindings"
echo "# "

cd "$SCRIPT_DIR/minisat-c-bindings"
make

echo "# "
echo "# Creating symlinks for shared library"
echo "# "

cd "$SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib"
ln -sf libminisat-c.so.1.0.0 libminisat-c.so
ln -sf libminisat-c.so.1.0.0 libminisat-c.so.1

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cd "$PROJECT_ROOT"

cabal run hs-bindgen-cli -- \
    preprocess \
    -I "$SCRIPT_DIR/minisat-c-bindings" \
    --hs-output-dir "$SCRIPT_DIR/hs-project/src" \
    --module Minisat.Generated \
    "$SCRIPT_DIR/minisat-c-bindings/minisat.h"

echo "# "
echo "# Creating cabal.project.local"
echo "# "

cat > "$SCRIPT_DIR/hs-project/cabal.project.local" <<EOF
package c-minisat
    extra-include-dirs:
        $SCRIPT_DIR/minisat-c-bindings
      , $SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib
    extra-lib-dirs:
        $SCRIPT_DIR/minisat-c-bindings
      , $SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib
EOF

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"
cd $SCRIPT_DIR/hs-project
export LD_LIBRARY_PATH=$SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib:\$LD_LIBRARY_PATH
cabal build
cabal run c-minisat
