#!/usr/bin/env bash

set -e

nix_flakes_equal_revision() {
    grep "github:well-typed/libclang" "${NIX_FLAKE}" |
        grep "${REV_OLD}" \
            >/dev/null 2>&1
}

nix_flakes_enabled() {
    nix config show |
        grep experimental-features |
        grep flakes \
            >/dev/null 2>&1
}

PROJECT_ROOT=$(git rev-parse --show-toplevel)
CABAL_PROJECT_BASE="${PROJECT_ROOT}/cabal.project.base"
NIX_FLAKE="${PROJECT_ROOT}/flake.nix"

REV_OLD=$(grep -C 2 'github.com/well-typed/libclang' "${CABAL_PROJECT_BASE}" |
    grep 'tag:' |
    awk '{print $NF}')

if [ -z "$REV_OLD" ]; then
    echo "Error: Could not find current libclang tag in $CABAL_PROJECT_BASE"
    exit 1
fi

if nix_flakes_equal_revision; then
    echo "Old libclang-bindings revision: ${REV_OLD}"
else
    echo "Error: The Cabal and Nix 'libclang-bindings' revision have diverged"
    exit 1
fi

REV_NEW=$(git ls-remote https://github.com/well-typed/libclang HEAD | cut -f 1)
echo "New libclang-bindings revision: ${REV_NEW}"

sed -i 's/'"${REV_OLD}"'/'"${REV_NEW}"'/' "$CABAL_PROJECT_BASE"
sed -i 's/'"${REV_OLD}"'/'"${REV_NEW}"'/' "$NIX_FLAKE"

if command -v nix >/dev/null 2>&1; then
    if nix_flakes_enabled; then
        nix flake update libclang-bindings-src
    else
        echo "Warning: Nix Flakes are not enabled"
        echo "         We advise to pull along the Nix Flake lock file"
    fi
else
    echo "Warning: The command 'nix' is not installed"
    echo "         We advise to pull along the Nix Flake lock file"
fi
