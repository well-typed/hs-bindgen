#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# rogueutil library, pinned so the example is reproducible. We fetch it on
# demand rather than as a git submodule: a submodule is cloned by cabal for
# every project that depends on hs-bindgen via source-repository-package, even
# though only hs-bindgen-runtime is needed.
ROGUEUTIL_REPO="https://github.com/sakhmatd/rogueutil.git"
ROGUEUTIL_COMMIT="bbbc1ef73e9df6d22a3459f92f7e16bd8be535f5"

echo "# "
echo "# Building rogueutil C bindings"
echo "# "

if [ ! -d "$SCRIPT_DIR/rogueutil/.git" ]; then
    # --filter=blob:none keeps the download small (blobs are fetched on demand)
    # while still allowing checkout of the pinned commit; a shallow clone could
    # not check out an arbitrary older commit.
    git clone --filter=blob:none "$ROGUEUTIL_REPO" "$SCRIPT_DIR/rogueutil"
fi
git -C "$SCRIPT_DIR/rogueutil" checkout --quiet "$ROGUEUTIL_COMMIT"

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
