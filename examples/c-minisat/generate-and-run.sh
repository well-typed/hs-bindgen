#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# MiniSat C bindings, pinned so the example is reproducible. We fetch it on
# demand rather than as a git submodule: a submodule is cloned by cabal for
# every project that depends on hs-bindgen via source-repository-package, even
# though only hs-bindgen-runtime is needed.
MINISAT_REPO="https://github.com/niklasso/minisat-c-bindings.git"
MINISAT_COMMIT="2f137cbedc7a1a0ecd4117baaf84a005c2045134"

(
    echo "# "
    echo "# Building minisat C bindings"
    echo "# "

    if [ ! -d "$SCRIPT_DIR/minisat-c-bindings/.git" ]; then
        # --filter=blob:none keeps the download small (blobs are fetched on
        # demand) while still allowing checkout of the pinned commit; a shallow
        # clone could not check out an arbitrary older commit.
        git clone --filter=blob:none "$MINISAT_REPO" "$SCRIPT_DIR/minisat-c-bindings"
    fi
    git -C "$SCRIPT_DIR/minisat-c-bindings" checkout --quiet "$MINISAT_COMMIT"

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

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I "minisat-c-bindings" \
    --hs-output-dir "hs-project/src" \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Minisat \
    "minisat.h"

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(
    cat <<-EOF
package c-minisat
    extra-include-dirs:
        $SCRIPT_DIR/minisat-c-bindings
      , $SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib
    extra-lib-dirs:
        $SCRIPT_DIR/minisat-c-bindings
      , $SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib
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

    cd "$SCRIPT_DIR/hs-project"
    LD_LIBRARY_PATH="$SCRIPT_DIR/minisat-c-bindings/build/dynamic/lib:$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH
    cabal build c-minisat-bin
    cabal run c-minisat-bin
)
