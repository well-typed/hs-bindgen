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

echo "# "
echo "# Yes stdlib"
echo "# "

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated.Stdlib.A \
    --select-by-decl-name "^A$" \
    --gen-binding-spec external/stdlib/A.bindingspec.yaml \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated.Stdlib.B \
    --select-by-decl-name "^B$" \
    --gen-binding-spec external/stdlib/B.bindingspec.yaml \
    --external-binding-spec external/stdlib/A.bindingspec.yaml \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated.Stdlib.C \
    --select-by-decl-name "^ex*" \
    --gen-binding-spec external/stdlib/C.bindingspec.yaml \
    --external-binding-spec external/stdlib/A.bindingspec.yaml \
    --external-binding-spec external/stdlib/B.bindingspec.yaml \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

echo "# "
echo "# No stdlib"
echo "# "

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated.NoStdlib.A \
    --enable-program-slicing \
    --select-by-decl-name "^A$" \
    --gen-binding-spec external/no-stdlib/A.bindingspec.yaml \
    --no-stdlib \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated.NoStdlib.B \
    --select-by-decl-name "^B$" \
    --gen-binding-spec external/no-stdlib/B.bindingspec.yaml \
    --external-binding-spec external/no-stdlib/A.bindingspec.yaml \
    --no-stdlib \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id constructor-import-issue.well-typed.com \
    --hs-output-dir hs-project/generated \
    --module Generated.NoStdlib.C \
    --select-by-decl-name "^ex*" \
    --gen-binding-spec external/no-stdlib/C.bindingspec.yaml \
    --external-binding-spec external/no-stdlib/A.bindingspec.yaml \
    --external-binding-spec external/no-stdlib/B.bindingspec.yaml \
    --no-stdlib \
    --create-output-dirs \
    --overwrite-files \
    constructor_import_issue.h

echo "# "
echo "# Generating cabal.project.paths"
echo "# "

cat > "$SCRIPT_DIR/hs-project/cabal.project.paths" <<EOF
package constructor-import-issue
    extra-include-dirs: $C_DIR
    extra-lib-dirs: $C_DIR
EOF
cat "$SCRIPT_DIR/hs-project/cabal.project.paths"

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
