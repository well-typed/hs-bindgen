#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

(
    echo "# "
    echo "# Building C library"
    echo "# "

    cd "$SCRIPT_DIR/c"
    make
)

C_DIR=$(realpath c)
echo $C_DIR

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/generated \
    --unique-id blocksdemo.well-typed.com \
    --module Iterator \
    --standard c23 \
    --enable-blocks \
    iterator.h

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(cat <<-EOF
package blocktest
    extra-include-dirs: $C_DIR
    extra-lib-dirs: $C_DIR
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

    LD_LIBRARY_PATH="$C_DIR:$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH
    echo "LD_LIBRARY_PATH: $LD_LIBRARY_PATH"

    cd "hs-project"
    cabal build
    cabal run
)
