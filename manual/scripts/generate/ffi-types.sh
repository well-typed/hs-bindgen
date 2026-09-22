#!/usr/bin/env bash

# Exit on first error
set -e

echo "# "
echo "# Manual: FFI types"
echo "# "

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MANUAL_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
PROJECT_ROOT="$(cd "$MANUAL_DIR/.." && pwd)"

SPEC_GEN_DIR="$MANUAL_DIR/binding-specs/ffi-types"

if [ -d "$SPEC_GEN_DIR" ]; then
  rm -r "$SPEC_GEN_DIR"
fi
mkdir -p "$SPEC_GEN_DIR"

EXTERNAL_TYPES_DIR="$MANUAL_DIR/hs/ffi-types/src-external-types"
GENERATED_DIR="$MANUAL_DIR/hs/ffi-types/src-generated"

if [ -d "$GENERATED_DIR" ]; then
  rm -r "$GENERATED_DIR"
fi
mkdir -p "$GENERATED_DIR"

UNIQUE_ID=com.hs-bindgen.ffi-types

echo "## Example 1"

cabal run --project-dir="${PROJECT_ROOT}" hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id "$UNIQUE_ID" \
    --hs-output-dir "$GENERATED_DIR" \
    --module Example1 \
    --select-by-decl-name "^A$|^B$|^foo*" \
    --gen-binding-spec "$SPEC_GEN_DIR/example1.bindingspec.yaml" \
    --create-output-dirs \
    --overwrite-files \
    ffi-types/int.h

echo "## Example 2"

cabal run --project-dir="${PROJECT_ROOT}" hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id "$UNIQUE_ID" \
    --hs-output-dir "$GENERATED_DIR" \
    --module Example2.A \
    --select-by-decl-name "^A$" \
    --gen-binding-spec "$SPEC_GEN_DIR/example2/A.bindingspec.yaml" \
    --create-output-dirs \
    --overwrite-files \
    ffi-types/int.h

cabal run --project-dir="${PROJECT_ROOT}" hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id "$UNIQUE_ID" \
    --hs-output-dir "$GENERATED_DIR" \
    --module Example2.B \
    --select-by-decl-name "^B$" \
    --gen-binding-spec "$SPEC_GEN_DIR/example2/B.bindingspec.yaml" \
    --external-binding-spec "$SPEC_GEN_DIR/example2/A.bindingspec.yaml" \
    --create-output-dirs \
    --overwrite-files \
    ffi-types/int.h

cabal run --project-dir="${PROJECT_ROOT}" hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id "$UNIQUE_ID" \
    --hs-output-dir "$GENERATED_DIR" \
    --module Example2.C \
    --select-by-decl-name "^foo*" \
    --gen-binding-spec "$SPEC_GEN_DIR/example2/C.bindingspec.yaml" \
    --external-binding-spec "$SPEC_GEN_DIR/example2/A.bindingspec.yaml" \
    --external-binding-spec "$SPEC_GEN_DIR/example2/B.bindingspec.yaml" \
    --create-output-dirs \
    --overwrite-files \
    ffi-types/int.h

echo "## Example 3"

cabal run --project-dir="${PROJECT_ROOT}" hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id "$UNIQUE_ID" \
    --hs-output-dir "$GENERATED_DIR" \
    --module Example3.B \
    --select-by-decl-name "^B$" \
    --gen-binding-spec "$SPEC_GEN_DIR/example3/B.bindingspec.yaml" \
    --external-binding-spec "$EXTERNAL_TYPES_DIR/Example3/A.bindingspec.yaml" \
    --create-output-dirs \
    --overwrite-files \
    ffi-types/int.h

cabal run --project-dir="${PROJECT_ROOT}" hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id "$UNIQUE_ID" \
    --hs-output-dir "$GENERATED_DIR" \
    --module Example3.C \
    --select-by-decl-name "^foo*" \
    --gen-binding-spec "$SPEC_GEN_DIR/example3/C.bindingspec.yaml" \
    --external-binding-spec "$EXTERNAL_TYPES_DIR/Example3/A.bindingspec.yaml" \
    --external-binding-spec "$SPEC_GEN_DIR/example3/B.bindingspec.yaml" \
    --create-output-dirs \
    --overwrite-files \
    ffi-types/int.h

echo "## Example 4"

cabal run --project-dir="${PROJECT_ROOT}" hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id "$UNIQUE_ID" \
    --hs-output-dir "$GENERATED_DIR" \
    --module Example4.B \
    --select-by-decl-name "^B$" \
    --gen-binding-spec "$SPEC_GEN_DIR/example4/B.bindingspec.yaml" \
    --external-binding-spec "$EXTERNAL_TYPES_DIR/Example4/A.bindingspec.yaml" \
    --create-output-dirs \
    --overwrite-files \
    ffi-types/int.h

cabal run --project-dir="${PROJECT_ROOT}" hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --unique-id "$UNIQUE_ID" \
    --hs-output-dir "$GENERATED_DIR" \
    --module Example4.C \
    --select-by-decl-name "^foo*" \
    --gen-binding-spec "$SPEC_GEN_DIR/example4/C.bindingspec.yaml" \
    --external-binding-spec "$EXTERNAL_TYPES_DIR/Example4/A.bindingspec.yaml" \
    --external-binding-spec "$SPEC_GEN_DIR/example4/B.bindingspec.yaml" \
    --create-output-dirs \
    --overwrite-files \
    ffi-types/int.h
