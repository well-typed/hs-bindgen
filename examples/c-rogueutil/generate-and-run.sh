#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "# "
echo "# Building rogueutil C bindings"
echo "# "

cd "$SCRIPT_DIR/rogueutil"
make

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cd "$PROJECT_ROOT"

cabal run hs-bindgen-cli -- \
    preprocess \
    -I "$SCRIPT_DIR/rogueutil" \
    --create-output-dirs \
    --overwrite-files \
    --single-file \
    --safe "" \
    --hs-output-dir "$SCRIPT_DIR/hs-project/src" \
    --module RogueUtil.Generated \
    --clang-option=-D_POSIX_C_SOURCE=200809L \
    "$SCRIPT_DIR/rogueutil/rogueutil.h"

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(
    cat <<-EOF
package c-rogueutil
    extra-include-dirs:
        $SCRIPT_DIR/rogueutil
    extra-lib-dirs:
        $SCRIPT_DIR/rogueutil
EOF
)
grep -qxF "$LINE" "$SCRIPT_DIR/hs-project/cabal.project.local" || echo "$LINE" >>"$SCRIPT_DIR/hs-project/cabal.project.local"
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"

cd $SCRIPT_DIR/hs-project
export LD_LIBRARY_PATH=$SCRIPT_DIR/rogueutil/:\$LD_LIBRARY_PATH

cabal build
cabal run c-rogueutil
