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
# NOTE: Mirrored in Test.HsBindgen.THFixtures.TestCases.determineTHStatus
KNOWN_FAILURES=(
    binding-specs/fun_arg/typedef/array               # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/array_known_size    # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/enum                # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/function            # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/function_pointer    # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/struct              # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/typedef/union               # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/array                 # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/array_known_size      # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/enum                  # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/function              # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/function_pointer      # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/struct                # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    binding-specs/fun_arg/macro/union                 # Fixtures with external binding specs can not be compiled yet (see issue #1495)
    edge-cases/iterator                               # Makes use of Apple block extension which would require clang (see #913)
    functions/decls_in_signature                      # Unusable struct (see #1128)
    functions/heap_types/struct_const_member          # Issue #1490
    functions/heap_types/struct_const_typedef         # Issue #1490
    functions/heap_types/struct_const                 # Issue #1490
    functions/heap_types/union_const_member           # Issue #1490
    functions/heap_types/union_const_typedef          # Issue #1490
    functions/heap_types/union_const                  # Issue #1490
    program-analysis/program-slicing/macro_selected   # TODO: see issue #1679
    program-analysis/program-slicing/macro_unselected # TODO: see issue #1679
)

# Known fixtures without code - these will be skipped
# NOTE: Mirrored in Test.HsBindgen.THFixtures.TestCases.emptyOutputFixtures
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

# Known fixtures that compile, but not cleanly, so they should be run without
# -Werror
KNOWN_WERROR_UNCLEAN=(
    arrays/array
    edge-cases/adios
    attributes/visibility_attributes
    declarations/tentative_definitions
)

# The number of fixtures that are known to exist (including known failures)
#
# This number is used for sanity checks. Make sure to update this number when
# new fixtures are added or old ones are removed.
KNOWN_FIXTURES_COUNT=168

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
SHARED_BUILD_DIR="$REPO_ROOT/dist-newstyle-fixture-check"

# Verify directories exist
if [[ ! -d "$FIXTURES_DIR" ]]; then
    echo "Error: Fixtures not found at $FIXTURES_DIR" >&2
    exit 1
fi

###############################################################################
# Helper functions
###############################################################################

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
    for unclean in "${KNOWN_WERROR_UNCLEAN[@]}"; do
        if [[ "$fixture_name" == "$unclean" ]]; then
            return 0
        fi
    done
    return 1
}

# Sanitize a fixture name for use as a cabal library name
#
# We replace /, _ with - and handle . and _ before digits specially: a dot or
# underscore followed by a digit gets the digit prefixed with 'v' or 'n'
# respectively (e.g. ".1." -> "v1-", "_1" -> "n1") to avoid creating segments
# like "-1-" or trailing "-1" which cabal's munged package ID parser
# misinterprets as version numbers. Remaining / _ . become -.
sanitize() {
    echo "$1" | sed 's/\.\([0-9]\)/v\1/g; s/_\([0-9]\)/n\1/g' | tr '/_.' '---'
}

# Detect Haskell modules in a fixture directory
#
# Given the fixture directory, finds all .hs files and converts paths to module
# names: Example.hs -> Example, Example/Safe.hs -> Example.Safe
detect_modules() {
    local fixture_dir="$1"
    find "$fixture_dir" -type f -name "*.hs" -print0 |
        sort -z |
        while IFS= read -r -d '' hs_file; do
            local rel="${hs_file#"$fixture_dir/"}"
            # Strip .hs extension, replace / with .
            rel="${rel%.hs}"
            echo "${rel//\//.}"
        done
}

###############################################################################
# Cabal file and project generation
###############################################################################

# Generate hs-bindgen-fixtures.cabal in the given directory
#
# The generated package uses common stanzas and internal libraries. All paths
# are absolute so the package can live in a temp directory outside the repo.
generate_cabal_file() {
    local target_dir="$1"
    local cabal_file="$target_dir/hs-bindgen-fixtures.cabal"

    {
        cat <<HEADER
cabal-version: 3.0
name:          hs-bindgen-fixtures
version:       0.0.0
build-type:    Simple

-- This file is generated by scripts/ci/compile-fixtures.sh
-- Do not edit manually.

common fixture-common
  default-language: GHC2021
  ghc-options:
    -Wall
    -Werror
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wmissing-exported-signatures
    -Widentities
    -Wredundant-constraints
    -Wpartial-fields
    -Wcpp-undef
    -Wno-unused-matches
    -optc-std=gnu2x
    -optc-Wno-deprecated-declarations
    -optc-Wno-attributes
  build-depends:
    , base
    , hs-bindgen-runtime
    , c-expr-runtime
  include-dirs:
    ${REPO_ROOT}/hs-bindgen/examples
    ${REPO_ROOT}/hs-bindgen/examples/golden
    ${REPO_ROOT}/hs-bindgen/musl-include/x86_64

common fixture-cc-werror
  ghc-options: -optc-Werror
HEADER

        # Generate one library per fixture
        while IFS= read -r -d '' bindingspec; do
            local fixture_dir
            fixture_dir=$(dirname "$bindingspec")
            local fixture_name
            fixture_name="${fixture_dir#"$FIXTURES_DIR/"}"
            fixture_name="${fixture_name%/}"

            # Skip empty fixtures
            local modules
            modules=$(detect_modules "$fixture_dir")
            if [[ -z "$modules" ]]; then
                continue
            fi

            # Skip known failures (unless forced)
            if [[ "$FORCE_ALL" == "false" ]] && is_known_failure "$fixture_name"; then
                continue
            fi

            # Skip known empty (unless forced)
            if [[ "$FORCE_ALL" == "false" ]] && is_known_empty "$fixture_name"; then
                continue
            fi

            local lib_name
            lib_name="fixture-$(sanitize "$fixture_name")"

            # Determine which common stanzas to import
            local imports="fixture-common"
            if [[ "$WERROR_ALL" == "true" ]] || ! is_known_werror_unclean "$fixture_name"; then
                imports="fixture-common, fixture-cc-werror"
            fi

            echo ""
            echo "library $lib_name"
            echo "  import: $imports"
            echo "  hs-source-dirs: $REPO_ROOT/hs-bindgen/fixtures/$fixture_name"
            echo "  exposed-modules:"
            while IFS= read -r mod; do
                if [[ -n "$mod" ]]; then
                    echo "    $mod"
                fi
            done <<< "$modules"
        done < <(find "$FIXTURES_DIR" -type f -name "bindingspec.yaml" -print0 | sort -z)

    } > "$cabal_file"
}

# Generate a self-contained cabal.project in the given directory
#
# Uses the index-state from cabal.project.base and references local packages
# (hs-bindgen-runtime, c-expr-runtime, c-expr-dsl) by absolute path so that
# cabal can resolve build-depends without cloning or importing the full project.
generate_cabal_project() {
    local target_dir="$1"
    local index_state
    index_state=$(grep '^index-state:' "$REPO_ROOT/cabal.project.base" | head -1)
    cat > "$target_dir/cabal.project" <<PROJECT
${index_state}
packages:
  .
  ${REPO_ROOT}/hs-bindgen-runtime
  ${REPO_ROOT}/c-expr-runtime
  ${REPO_ROOT}/c-expr-dsl
allow-newer: all
package hs-bindgen-fixtures
  shared: False
PROJECT
}

###############################################################################
# Main
###############################################################################

# Create temp dir for generated package; clean up on exit
BATCH_DIR=$(mktemp -d)
trap 'rm -rf "$BATCH_DIR"' EXIT

# Single fixture mode
if [[ $# -eq 1 ]]; then
    echo "========================================="
    echo "Compiling single fixture: $1"
    echo "========================================="
    echo ""

    generate_cabal_file "$BATCH_DIR"
    generate_cabal_project "$BATCH_DIR"

    lib_name="fixture-$(sanitize "$1")"

    BUILD_EXIT=0
    (cd "$BATCH_DIR" && cabal build "hs-bindgen-fixtures:$lib_name" \
        --builddir="$SHARED_BUILD_DIR") || BUILD_EXIT=$?

    echo ""
    echo "========================================="
    if [[ $BUILD_EXIT -eq 0 ]]; then
        echo "✓ $1"
    else
        echo "✗ $1"
    fi
    echo "========================================="
    exit $BUILD_EXIT
fi

# Batch mode: collect fixtures
echo "Collecting fixtures..."
FIXTURES_TO_COMPILE=()
FIXTURES_SKIPPED=()
FIXTURES_EMPTY=()

# Use find to recursively search for all fixtures
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

# Generate cabal package and project in temp dir
generate_cabal_file "$BATCH_DIR"
generate_cabal_project "$BATCH_DIR"

echo "Compiling ${#FIXTURES_TO_COMPILE[@]} fixtures..."
echo ""

# Build all fixtures with --keep-going to report all failures at once.
# Use tee so that cabal's output (progress, warnings, errors) is visible in
# real time while also being captured for post-build per-fixture parsing.
BUILD_LOG="$BATCH_DIR/build.log"
BUILD_EXIT=0
(cd "$BATCH_DIR" && cabal build hs-bindgen-fixtures \
    -j"$JOBS" --keep-going \
    --builddir="$SHARED_BUILD_DIR" \
    2>&1) | tee "$BUILD_LOG" || BUILD_EXIT=$?

# Extract failed library names from the build log
#
# Cabal reports failures in lines like:
#   Failed to build hs-bindgen-fixtures-0.0.0 (lib:fixture-xxx from ...)
FAILED_LIBS=()
while IFS= read -r line; do
    if [[ "$line" =~ lib:(fixture-[a-zA-Z0-9_-]+) ]]; then
        FAILED_LIBS+=("${BASH_REMATCH[1]}")
    fi
done < <(grep "Failed to build" "$BUILD_LOG" 2>/dev/null || true)

# Check if a library is in the failed set
is_lib_failed() {
    local lib="$1"
    local f
    for f in "${FAILED_LIBS[@]+"${FAILED_LIBS[@]}"}"; do
        if [[ "$f" == "$lib" ]]; then
            return 0
        fi
    done
    return 1
}

# Classify per-fixture results
PASS=0
FAIL=0
FAIL_NAMES=()

for fixture_name in "${FIXTURES_TO_COMPILE[@]}"; do
    lib_name="fixture-$(sanitize "$fixture_name")"
    if is_lib_failed "$lib_name"; then
        FAIL=$((FAIL + 1))
        FAIL_NAMES+=("$fixture_name")
    else
        PASS=$((PASS + 1))
    fi
done

echo ""
echo "========================================="
echo "Results: $PASS passed, $FAIL failed, ${#FIXTURES_SKIPPED[@]} skipped"
echo "========================================="

if [[ ${#FAIL_NAMES[@]} -gt 0 ]]; then
    echo ""
    echo "Failed fixtures:"
    for name in "${FAIL_NAMES[@]}"; do
        echo "  ✗ $name"
    done
fi

exit $BUILD_EXIT
