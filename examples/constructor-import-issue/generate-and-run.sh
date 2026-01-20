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

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated.A \
    --select-by-decl-name "A" \
    --gen-binding-spec external/A.bindingspec.yaml \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated.B \
    --select-by-decl-name "B" \
    --gen-binding-spec external/B.bindingspec.yaml \
    --external-binding-spec external/A.bindingspec.yaml \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated \
    --external-binding-spec external/A.bindingspec.yaml \
    --external-binding-spec external/B.bindingspec.yaml \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

echo "# "
echo "# Updating cabal.project.local"
echo "# "

LINE=$(cat <<-EOF
package constructor-import-issue
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
