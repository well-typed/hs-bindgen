#!/usr/bin/env bash

set -euo pipefail

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Compile generated .pp.hs fixture files to verify they produce valid Haskell code.

Options:
  -j N    Number of parallel jobs (default: 4)
  -f      Force compilation of all fixtures, including known failures
  -h      Show this help message

Exit codes:
  0       All (non-skipped) fixtures compiled successfully
  1       One or more fixtures failed to compile
EOF
}

# Known failures - these will be skipped unless -f is used
KNOWN_FAILURES=(
    # GCC Failures
    iterator.pp.hs # Makes use of Apple block extension which would require clang (see #913)
    decls_in_signature.pp.hs # Unusable struct (See #1128)
    redeclaration.pp.hs # Same as typenames.pp.hs
    visibility_attributes.pp.hs

    # GHC Failures
    typenames.pp.hs # hs-bindgen namespace possible bug/feature
)

# Default options
JOBS=4
FORCE_ALL=false

# Parse options
while getopts "j:fh" opt; do
    case "$opt" in
        j)
            JOBS="$OPTARG"
            ;;
        f)
            FORCE_ALL=true
            ;;
        h)
            usage
            exit 0
            ;;
        *)
            usage >&2
            exit 1
            ;;
    esac
done
shift $((OPTIND - 1))

# Detect script location and set up paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HS_BINDGEN_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
EXAMPLES_DIR="$HS_BINDGEN_DIR/examples"

# Verify directories exist
if [[ ! -f "$SCRIPT_DIR/adios.pp.hs" ]]; then
    echo "Error: Fixtures not found at $SCRIPT_DIR" >&2
    exit 1
fi

# Function to check if a file is in the known failures list
is_known_failure() {
    local file="$1"
    local basename_file
    basename_file=$(basename "$file")

    for failure in "${KNOWN_FAILURES[@]}"; do
        if [[ "$basename_file" == "$failure" ]]; then
            return 0
        fi
    done
    return 1
}

# Function to compile a single fixture
compile_fixture() {
    local file="$1"
    local basename_file
    basename_file=$(basename "$file")

    # Use a temporary output file to avoid polluting the fixtures directory
    local output_dir
    output_dir=$(mktemp -d)

    # Build extra clang args from BINDGEN_EXTRA_CLANG_ARGS environment variable
    # This variable is set by cabal exec and contains all necessary paths for Clang
    # to find system headers in the Nix environment
    local extra_clang_args=()
    if [[ -n "${BINDGEN_EXTRA_CLANG_ARGS:-}" ]]; then
        # Parse space-separated args and prepend each with -optc
        while IFS= read -r arg; do
            [[ -n "$arg" ]] && extra_clang_args+=("-optc" "$arg")
        done < <(echo "$BINDGEN_EXTRA_CLANG_ARGS" | xargs -n1)
    fi

    # Compile the fixture with GHC
    # -c: Compile only (no linking)
    # -fforce-recomp: Always recompile
    # -package: Make package available for import
    # -outputdir: Where to put build artifacts
    # -pgmc clang: Use Clang as the C compiler (better C23/Blocks support than GCC, see #913)
    # -optc: Pass the following flag to the C compiler
    #   -I: Add include directory for C headers
    #   -std=gnu23: Use GNU C23 standard (supports bool type + GNU extensions like asm)
    #   -fblocks: Enable Apple Blocks extension (closures in C)
    #   -Wno-deprecated-declarations: Suppress warnings about deprecated functions
    #   -Wno-attributes: Suppress warnings about unrecognized or ignored attributes
    #   ${extra_clang_args[@]}: Additional flags from BINDGEN_EXTRA_CLANG_ARGS (system include paths)
    if (cd "$HS_BINDGEN_DIR" && cabal exec -- ghc \
        -c \
        -fforce-recomp \
        -outputdir "$output_dir" \
        -package hs-bindgen-runtime \
        -package c-expr-runtime \
        -optc -I"$EXAMPLES_DIR" \
        -optc -I"$EXAMPLES_DIR/golden" \
        -optc -std=gnu23 \
        -optc -Wno-deprecated-declarations \
        -optc -Wno-attributes \
        "$file" &>"$output_dir/compile.log"); then
        echo "✓ $basename_file"
        rm -rf "$output_dir"
        return 0
    else
        echo "✗ $basename_file"
        if [[ -s "$output_dir/compile.log" ]]; then
            echo "  Error log:"
            sed 's/^/    /' "$output_dir/compile.log"
        fi
        rm -rf "$output_dir"
        return 1
    fi
}

# Make these functions and variables available to child processes (subshells)
export -f compile_fixture
export -f is_known_failure
export KNOWN_FAILURES
export HS_BINDGEN_DIR
export EXAMPLES_DIR

# Collect fixtures to compile
echo "Collecting fixtures..."
FIXTURES_TO_COMPILE=()
FIXTURES_SKIPPED=()

for file in "$SCRIPT_DIR"/*.pp.hs; do
    [[ -f "$file" ]] || continue

    if [[ "$FORCE_ALL" == "false" ]] && is_known_failure "$file"; then
        FIXTURES_SKIPPED+=("$file")
    else
        FIXTURES_TO_COMPILE+=("$file")
    fi
done

echo ""
echo "========================================="
echo "Fixture Compilation Report"
echo "========================================="
echo "Total fixtures: $(ls -1 "$SCRIPT_DIR"/*.pp.hs 2>/dev/null | wc -l)"
echo "To compile: ${#FIXTURES_TO_COMPILE[@]}"
echo "Skipped (known failures): ${#FIXTURES_SKIPPED[@]}"
echo "Parallel jobs: $JOBS"
echo "========================================="
echo ""

if [[ ${#FIXTURES_SKIPPED[@]} -gt 0 ]]; then
    echo "Skipped fixtures (use -f to force):"
    for file in "${FIXTURES_SKIPPED[@]}"; do
        echo "  - $file"
    done
    echo ""
fi

if [[ ${#FIXTURES_TO_COMPILE[@]} -eq 0 ]]; then
    echo "No fixtures to compile!"
    exit 0
fi

echo "Compiling ${#FIXTURES_TO_COMPILE[@]} fixtures with $JOBS parallel jobs..."
echo ""

# Compile fixtures in parallel using xargs
FAILED=0
if printf '%s\n' "${FIXTURES_TO_COMPILE[@]}" | xargs -P "$JOBS" -I {} bash -c 'compile_fixture "$@"' _ {}; then
    :
else
    FAILED=1
fi

echo ""
echo "========================================="
if [[ $FAILED -eq 0 ]]; then
    echo "✓ All fixtures compiled successfully!"
    echo "========================================="
    exit 0
else
    echo "✗ Some fixtures failed to compile"
    echo "========================================="
    exit 1
fi
