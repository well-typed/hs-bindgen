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

rm -r hs-project/src-generated/Generated

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Callbacks.Operators \
    callbacks/operators.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Callbacks.Stream \
    callbacks/stream.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Callbacks.View \
    callbacks/view.h

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(cat <<-EOF
package feature-tests
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
