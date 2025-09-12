#!/usr/bin/env bash

set -euo pipefail

echo "Formatting Haskell imports with stylish-haskell..."

# Check if stylish-haskell is installed
if ! command -v stylish-haskell >/dev/null 2>&1; then
    echo "stylish-haskell is not installed. Install it with:"
    echo "  cabal install stylish-haskell"
    exit 1
fi

# Run stylish-haskell on all files (will update them in place)
scripts/ci/run-stylish-haskell.sh -g

echo "Import formatting complete!"
echo "Review the changes with: git diff"
