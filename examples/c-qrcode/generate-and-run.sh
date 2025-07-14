#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "# "
echo "# Building qrcode C bindings"
echo "# "

cd "$SCRIPT_DIR/QR-Code-generator/c"
make

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cd "$PROJECT_ROOT"

cabal run hs-bindgen-cli -- \
    preprocess \
    -I "$SCRIPT_DIR/QR-Code-generator" \
    --hs-output-dir "$SCRIPT_DIR/hs-project/src" \
    --module QRCodeGenerator.Generated \
    "$SCRIPT_DIR/QR-Code-generator/c/qrcodegen.h"

echo "# "
echo "# Creating cabal.project.local"
echo "# "

cat > "$SCRIPT_DIR/hs-project/cabal.project.local" <<EOF
package c-qrcode
    extra-include-dirs:
        $SCRIPT_DIR/QR-Code-generator/c/
    extra-lib-dirs:
        $SCRIPT_DIR/QR-Code-generator/c/
EOF

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"

cd $SCRIPT_DIR/hs-project
export LD_LIBRARY_PATH=$SCRIPT_DIR/QR-Code-generator/c/:\$LD_LIBRARY_PATH

cabal build
cabal run c-qrcode
