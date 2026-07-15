#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

# Botan C++ library, pinned so the example is reproducible. We fetch it on
# demand rather than as a git submodule: a submodule is cloned by cabal for
# every project that depends on hs-bindgen via source-repository-package, even
# though only hs-bindgen-runtime is needed.
BOTAN_REPO="https://github.com/randombit/botan"
BOTAN_COMMIT="07e1cfe0a06b224bbb37ad534736924931184246"

(
    echo "# "
    echo "# Building botan"
    echo "# "

    if [ ! -d "$SCRIPT_DIR/botan/.git" ]; then
        # --filter=blob:none keeps the download small (blobs are fetched on
        # demand) while still allowing checkout of the pinned commit; a shallow
        # clone could not check out an arbitrary older commit.
        git clone --filter=blob:none "$BOTAN_REPO" "$SCRIPT_DIR/botan"
    fi
    git -C "$SCRIPT_DIR/botan" checkout --quiet "$BOTAN_COMMIT"

    cd "$SCRIPT_DIR/botan"
    ./configure.py --compiler-cache=ccache
    make
)

BOTAN_DIR=$(realpath botan)

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I "botan/build/include/public" \
    --hs-output-dir "hs-project/src" \
    --create-output-dirs \
    --overwrite-files \
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
