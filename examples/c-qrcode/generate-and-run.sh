#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# QR-Code-generator library, pinned to a release tag so the example is
# reproducible. We fetch it on demand rather than as a git submodule: a
# submodule is cloned by cabal for every project that depends on hs-bindgen via
# source-repository-package, even though only hs-bindgen-runtime is needed.
QRCODE_REPO="https://github.com/nayuki/QR-Code-generator.git"
QRCODE_TAG="v1.8.0"

echo "# "
echo "# Building qrcode C bindings"
echo "# "

if [ ! -d "$SCRIPT_DIR/QR-Code-generator/.git" ]; then
    # --filter=blob:none keeps the download small (blobs are fetched on demand)
    # while still allowing checkout of the pinned tag; a shallow clone could
    # not check out an arbitrary older tag.
    git clone --filter=blob:none "$QRCODE_REPO" "$SCRIPT_DIR/QR-Code-generator"
fi
git -C "$SCRIPT_DIR/QR-Code-generator" checkout --quiet "$QRCODE_TAG"

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
    --create-output-dirs \
    --overwrite-files \
    --module QRCodeGenerator.Generated \
    "$SCRIPT_DIR/QR-Code-generator/c/qrcodegen.h"

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(
    cat <<-EOF
package c-qrcode
    extra-include-dirs:
        $SCRIPT_DIR/QR-Code-generator/c/
    extra-lib-dirs:
        $SCRIPT_DIR/QR-Code-generator/c/
EOF
)
grep -qxF "$LINE" "$SCRIPT_DIR/hs-project/cabal.project.local" || echo "$LINE" >>"$SCRIPT_DIR/hs-project/cabal.project.local"
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"

cd $SCRIPT_DIR/hs-project
export LD_LIBRARY_PATH=$SCRIPT_DIR/QR-Code-generator/c/:\$LD_LIBRARY_PATH

cabal build
cabal run c-qrcode
