#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "# "
echo "# Building qrcode C bindings"
echo "# "

# Check out the submodule and build the C library only if it is not already
# built. Delete libqrcodegen.a to force a rebuild.
if [ -e "$SCRIPT_DIR/QR-Code-generator/c/libqrcodegen.a" ]; then
    echo "# libqrcodegen.a already built, skipping submodule checkout and make"
else
    git submodule update --init "$SCRIPT_DIR/QR-Code-generator"
    ( cd "$SCRIPT_DIR/QR-Code-generator/c" && make )
fi

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cd "$PROJECT_ROOT"

cabal run hs-bindgen-cli -- \
    preprocess \
    -I "$SCRIPT_DIR/QR-Code-generator" \
    --hs-output-dir "$SCRIPT_DIR/hs-project/src" \
    --create-output-dirs \
    --overwrite-files \
    --module QRCodeGenerator.Generated \
    "$SCRIPT_DIR/QR-Code-generator/c/qrcodegen.h"

echo "# "
echo "# Writing cabal.project.local"
echo "# "

# cabal.project.local is gitignored and machine-specific, so rewrite it from
# scratch each run rather than appending (which would accumulate stale paths).
cat > "$SCRIPT_DIR/hs-project/cabal.project.local" <<EOF
package c-qrcode
    extra-include-dirs:
        $SCRIPT_DIR/QR-Code-generator/c/
    extra-lib-dirs:
        $SCRIPT_DIR/QR-Code-generator/c/
EOF
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"

cd "$SCRIPT_DIR/hs-project"
export LD_LIBRARY_PATH="$SCRIPT_DIR/QR-Code-generator/c/:$LD_LIBRARY_PATH"

cabal build exe:c-qrcode
cabal run exe:c-qrcode
