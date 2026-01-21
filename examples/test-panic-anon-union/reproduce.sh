#!/usr/bin/env bash
# Reproduce the anonymous union panic with external binding specs

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
TEST_H="$SCRIPT_DIR/test.h"
OUTPUT_DIR="/tmp/test-anon-panic-output"
SPEC_DIR="/tmp/test-anon-panic-specs"

echo "=========================================="
echo "Reproducing anonymous union panic"
echo "=========================================="
echo

# Clean up any previous test output
rm -rf "$OUTPUT_DIR" "$SPEC_DIR"

# Create directories
mkdir -p "$OUTPUT_DIR" "$SPEC_DIR"

# Step 1: Generate binding spec for the anonymous union
echo "Step 1: Generating binding spec for anonymous union..."
cabal run hs-bindgen-cli -- preprocess \
    --hs-output-dir "$OUTPUT_DIR" \
    --create-output-dirs \
    --module Test.Outer \
    --parse-all \
    --select-from-main-headers \
    --enable-program-slicing \
    --gen-binding-spec "$SPEC_DIR/outer.yaml" \
    "$TEST_H"

echo
echo "Generated binding spec:"
cat "$SPEC_DIR/outer.yaml"
echo

# Step 2: Process using the external binding spec (this should panic)
echo "Step 2: Processing with external binding spec (should panic)..."
echo "Expected panic:"
echo "  PANIC!: the impossible happened"
echo "  Unexpected multiple locations for anon decl"
echo

if cabal run hs-bindgen-cli -- preprocess \
    --hs-output-dir "$OUTPUT_DIR" \
    --create-output-dirs \
    --module Test.Inner \
    --parse-all \
    --select-from-main-headers \
    --enable-program-slicing \
    --external-binding-spec "$SPEC_DIR/outer.yaml" \
    "$TEST_H" 2>&1; then
    echo
    echo "=========================================="
    echo "SUCCESS: No panic occurred!"
    echo "The bug may have been fixed."
    echo "=========================================="
else
    exit_code=$?
    echo
    echo "=========================================="
    echo "PANIC REPRODUCED (exit code: $exit_code)"
    echo "=========================================="
fi
