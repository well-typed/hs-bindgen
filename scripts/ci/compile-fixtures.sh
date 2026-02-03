#!/usr/bin/env bash

set -euo pipefail

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS] [FIXTURE]

Compile generated .hs fixture files to verify they produce valid Haskell code.

If a single FIXTURE is provided as an argument, only compile this FIXTURE.

Options:
  -j N    Number of parallel jobs (default: 4)
  -f      Force compilation of all fixtures, including known failures
  -w      Use -optc -Werror for the compilation of all fixtures, including known
          fixtures that do not compile cleanly with -optc -Werror
  -h      Show this help message

Exit codes:
  0       All (non-skipped) fixtures compiled successfully
  1       One or more fixtures failed to compile
EOF
}

# Known failures - these will be skipped unless -f is used
KNOWN_FAILURES=(
    binding-specs/fun_arg/typedef/array            # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/array_known_size # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/enum             # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/function         # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/function_pointer # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/struct           # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/union            # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/array              # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/array_known_size   # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/enum               # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/function           # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/function_pointer   # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/struct             # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/union              # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    edge-cases/iterator                            # Makes use of Apple block extension which would require clang (see #913)
    functions/decls_in_signature                   # Unusable struct (see #1128)
    functions/heap_types/struct_const_member       # Issue #1490
    functions/heap_types/struct_const_typedef      # Issue #1490
    functions/heap_types/struct_const              # Issue #1490
    functions/heap_types/union_const_member        # Issue #1490
    functions/heap_types/union_const_typedef       # Issue #1490
    functions/heap_types/union_const               # Issue #1490
)

# Known fixtures without code - these will be skipped
KNOWN_EMPTY=(
    binding-specs/macro_trans_dep_missing # TODO: issue #1513.
    declarations/declaration_unselected_b
    declarations/name_collision
    declarations/redeclaration_different
    edge-cases/clang_generated_collision
    edge-cases/duplicate
    edge-cases/headers
    edge-cases/select_no_match
    edge-cases/thread_local
    edge-cases/unsupported_builtin
    macros/macro_type_void
    program-analysis/delay_traces
    program-analysis/selection_foo
    program-analysis/selection_matches_c_names.2.negative_case
    program-analysis/selection_merge_traces
    program-analysis/selection_omit_prescriptive
    program-analysis/selection_squash_typedef
    types/special/long_double
    types/structs/implicit_fields_struct
    types/structs/unnamed-struct
    types/unions/implicit_fields_union
    types/typedefs/typenames
)

# Array variables can not be exported, so instead we export a function that sets
# the array variable, and that function can be used in subshells
set_known_werror_unclean() {
  # Known fixtures that compile, but not cleanly, so they should be run without
  # -Werror
  KNOWN_WERROR_UNLCEAN=(
      arrays/array
      edge-cases/adios
      attributes/visibility_attributes
      declarations/tentative_definitions
  )
}
export -f set_known_werror_unclean
set_known_werror_unclean

# The number of fixtures that are known to exist (including known failures)
#
# This number is used for sanity checks. Make sure to update this number when
# new fixtures are added or old ones are removed.
KNOWN_FIXTURES_COUNT=152

# Default options
JOBS=4
FORCE_ALL=false
WERROR_ALL=false

# Parse options
while getopts "j:fwh" opt; do
    case "$opt" in
    j)
        JOBS="$OPTARG"
        ;;
    f)
        FORCE_ALL=true
        ;;
    w)
        WERROR_ALL=true
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
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
HS_BINDGEN_DIR="$REPO_ROOT/hs-bindgen"
FIXTURES_DIR="$HS_BINDGEN_DIR/fixtures"
EXAMPLES_DIR="$HS_BINDGEN_DIR/examples"

# Verify directories exist
if [[ ! -d "$FIXTURES_DIR" ]]; then
    echo "Error: Fixtures not found at $FIXTURES_DIR" >&2
    exit 1
fi

# Extract the name of the fixture
#
# Examples:
# * Given a path of the form $FIXTURES_DIR/foo/Example.hs, returns foo
# * Given a path of the form $FIXTURES_DIR/manual/foo/Example.hs, returns manual/foo
get_fixture_name() {
    local file="$1"
    local fixture_dir
    fixture_dir=$(dirname "$file")
    local fixture_name
    fixture_name="${fixture_dir#"$FIXTURES_DIR/"}"
    fixture_name="${fixture_name%/}"
    echo "$fixture_name" >/dev/stdout
}

# Function to check if a file is in the known failures list
is_known_failure() {
    local fixture_name="$1"
    for failure in "${KNOWN_FAILURES[@]}"; do
        if [[ "$fixture_name" == "$failure" ]]; then
            return 0
        fi
    done
    return 1
}

# Function to check if a file is in the known empty fixtures list
is_known_empty() {
    local fixture_name="$1"
    for empty in "${KNOWN_EMPTY[@]}"; do
        if [[ "$fixture_name" == "$empty" ]]; then
            return 0
        fi
    done
    return 1
}

# Function to check if a file is in the known -Werror unclean fixtures list
is_known_werror_unclean() {
    local fixture_name="$1"
    for unclean in "${KNOWN_WERROR_UNLCEAN[@]}"; do
        if [[ "$fixture_name" == "$unclean" ]]; then
            return 0
        fi
    done
    return 1
}

# Function to compile a single fixture
# shellcheck disable=SC2329
compile_fixture() {
    local fixture_name="$1"

    # Given the name of the fixture, we search for all pretty-printed Haskell
    # files that we want to compile.
    #
    # NOTE: I (Joris) am not 100% sure, but it looks like the order in which the
    # files are passed to the GHC invocation matters for module dependency
    # resolution. We sort by directory depth first (shallower files first), then
    # alphabetically. This ensures Example.hs is compiled before Example/*.hs,
    # which is necessary since the submodules import the main Example module.
    local files
    files=$(find "$FIXTURES_DIR/$fixture_name/" -type f -name "*.hs" -print0 |
        xargs -0 -I {} sh -c 'echo $(echo "{}" | tr -cd "/" | wc -c) "{}"' |
        sort -n |
        cut -d' ' -f2- |
        tr '\n' ' ')

    # Use a temporary output file to avoid polluting the fixtures directory
    local output_dir
    output_dir=$(mktemp -d)

    local opts
    set_known_werror_unclean
    if [[ "$WERROR_ALL" == "true" ]]; then
      opts="-optc -Werror"
    elif is_known_werror_unclean "$fixture_name"; then
        opts=""
    else
        opts="-optc -Werror"
    fi

    # Compile the fixture with GHC
    # -c: Compile only (no linking)
    # -fforce-recomp: Always recompile
    # -package: Make package available for import
    # -outputdir: Where to put build artifacts
    # -optc: Pass the following flag to the C compiler
    #   -I: Add include directory for C headers
    #   -std=gnu2x: Use GNU C23 standard (supports C23 bool type + GNU extensions like asm)
    #   -Wno-deprecated-declarations: Suppress warnings about deprecated functions
    #   -Wno-attributes: Suppress warnings about unrecognized or ignored attributes
    if (cd "$HS_BINDGEN_DIR" && cabal exec -- ghc \
        -c \
        -fforce-recomp \
        -Wall \
        -Werror \
        -Wincomplete-uni-patterns \
        -Wincomplete-record-updates \
        -Wmissing-exported-signatures \
        -Widentities \
        -Wredundant-constraints \
        -Wpartial-fields \
        -Wcpp-undef \
        -Wno-unused-matches \
        -outputdir "$output_dir" \
        -package hs-bindgen-runtime \
        -package c-expr-runtime \
        -optc -I"$EXAMPLES_DIR" \
        -optc -I"$EXAMPLES_DIR/golden" \
        -optc -std=gnu2x \
        -optc -Wno-deprecated-declarations \
        -optc -Wno-attributes \
        "$opts" \
        $files &>"$output_dir/compile.log"); then
        echo "✓ $fixture_name"
        rm -rf "$output_dir"
        return 0
    else
        echo "✗ $fixture_name"
        if [[ -s "$output_dir/compile.log" ]]; then
            echo "  Error log:"
            sed 's/^/    /' "$output_dir/compile.log"
        fi
        rm -rf "$output_dir"
        return 1
    fi
}

if [ $# -eq 1 ]; then
    echo "========================================="
    echo "Compiling single fixture..."
    compile_fixture "$1"
    exit
fi

# Make these functions and variables available to child processes (subshells)
export -f compile_fixture
export -f is_known_werror_unclean
export WERROR_ALL
export KNOWN_FIXTURES_COUNT
export HS_BINDGEN_DIR
export EXAMPLES_DIR
export FIXTURES_DIR

# Collect fixtures to compile
echo "Collecting fixtures..."
FIXTURES_TO_COMPILE=()
FIXTURES_SKIPPED=()
FIXTURES_EMPTY=()

# Use find to recursively search for all .hs files
while IFS= read -r -d '' file; do
    fixture_name=$(get_fixture_name "$file")
    if [[ "$FORCE_ALL" == "false" ]] && is_known_failure "$fixture_name"; then
        FIXTURES_SKIPPED+=("$fixture_name")
    elif [[ "$FORCE_ALL" == "false" ]] && is_known_empty "$fixture_name"; then
        FIXTURES_EMPTY+=("$fixture_name")
    else
        FIXTURES_TO_COMPILE+=("$fixture_name")
    fi
done < <(find "$FIXTURES_DIR" -type f -name "bindingspec.yaml" -print0 | sort -z)

FIXTURES_FOUND_COUNT=$((${#FIXTURES_TO_COMPILE[@]} + ${#FIXTURES_SKIPPED[@]} + ${#FIXTURES_EMPTY[@]}))

echo ""
echo "========================================="
echo "Fixture Compilation Report"
echo "========================================="
echo "Total known fixtures: $KNOWN_FIXTURES_COUNT"
echo "Total found fixtures: $FIXTURES_FOUND_COUNT"
echo "To compile: ${#FIXTURES_TO_COMPILE[@]}"
echo "Skipped (known failures): ${#FIXTURES_SKIPPED[@]}"
echo "Skipped (empty): ${#FIXTURES_EMPTY[@]}"
echo "Parallel jobs: $JOBS"
echo "========================================="
echo ""

if [[ $KNOWN_FIXTURES_COUNT -ne $FIXTURES_FOUND_COUNT ]]; then
    echo "Error: total known fixtures is not equal to total found fixtures. Did you forget to update \$KNOWN_FIXTURES_COUNT?"
    exit 1
fi

if [[ ${#FIXTURES_SKIPPED[@]} -gt 0 ]]; then
    echo "Skipped fixtures (use -f to force):"
    for file in "${FIXTURES_SKIPPED[@]}"; do
        echo "  - $file"
    done
    echo ""
fi

if [[ ${#FIXTURES_EMPTY[@]} -gt 0 ]]; then
    echo "Empty fixtures (use -f to force):"
    for file in "${FIXTURES_EMPTY[@]}"; do
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
