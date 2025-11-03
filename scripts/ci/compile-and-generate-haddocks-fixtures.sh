#!/usr/bin/env bash
#
# compile-and-generate-haddocks-fixtures.sh
#
# This script serves a DUAL PURPOSE:
# 1. COMPILES all fixture files (validating they produce valid Haskell code)
# 2. GENERATES Haddock documentation with full dependency cross-referencing
#
# Since Haddock internally compiles the source files before generating documentation,
# running this script validates both compilation AND documentation generation in a
# single pass. This is more efficient than running separate compile and haddock passes.
#

set -euo pipefail

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Compile AND generate Haddock documentation for .pp.hs fixture files.

This script serves a dual purpose:
  1. Validates that all fixture files compile successfully
  2. Generates Haddock documentation with full dependency cross-referencing

Since Haddock compiles files internally before generating docs, this single script
accomplishes both goals efficiently in one pass.

Options:
  -j N    Number of parallel jobs (default: 4)
  -f      Force processing of all fixtures, including known failures
  -h      Show this help message

Exit codes:
  0       All (non-skipped) fixtures compiled and documented successfully
  1       One or more fixtures failed to compile or generate docs
EOF
}

# Known failures - these will be skipped unless -f is used
KNOWN_FAILURES=(
    iterator.pp.hs           # Makes use of Apple block extension which would require clang (see #913)
    decls_in_signature.pp.hs # Unusable struct (see #1128)
    redeclaration.pp.hs      # Same as typenames.pp.hs
    typenames.pp.hs          # hs-bindgen namespace possible bug/feature
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
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
HS_BINDGEN_DIR="$REPO_ROOT/hs-bindgen"
FIXTURES_DIR="$HS_BINDGEN_DIR/fixtures"
EXAMPLES_DIR="$HS_BINDGEN_DIR/examples"

# Verify directories exist
if [[ ! -d "$FIXTURES_DIR" || ! -f "$FIXTURES_DIR/adios.pp.hs" ]]; then
    echo "Error: Fixtures not found at $FIXTURES_DIR" >&2
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

# Function to compile and generate haddock for a single fixture
process_fixture() {
    local file="$1"
    local basename_file
    basename_file=$(basename "$file")

    # Extract fixture name without .pp.hs extension for output directory
    local fixture_name
    fixture_name=$(basename "$file" .pp.hs)

    # Use a persistent output directory so documentation can be browsed
    # This allows users to view all fixture haddocks together
    local output_dir
    output_dir="$HADDOCK_OUTPUT_DIR/$fixture_name"
    mkdir -p "$output_dir"

    # Compile the fixture and generate Haddock documentation with full dependency linking
    # Haddock will:
    #   1. Compile the source file using GHC (validates Haskell code)
    #   2. Generate HTML documentation (validates Haddock markup)
    #   3. Cross-reference with dependency packages (validates imports)
    #
    # --html: Generate HTML documentation
    # --odir: Output directory
    # --read-interface: Read dependency interface files for cross-referencing
    #   Format: <html-path>,<interface-file>
    # --optghc: Pass options to the GHC that haddock uses internally for compilation
    #   -package: Make package available for import
    #   -optc: Pass the following flag to the C compiler
    #     -I: Add include directory for C headers
    #     -std=gnu2x: Use GNU C23 standard (supports C23 bool type + GNU extensions like asm)
    #     -Wno-deprecated-declarations: Suppress warnings about deprecated functions
    #     -Wno-attributes: Suppress warnings about unrecognized or ignored attributes
    #
    if (cd "$HS_BINDGEN_DIR" && cabal exec -- haddock \
        --html \
        --odir="$output_dir" \
        --optghc="-package hs-bindgen-runtime" \
        --optghc="-package c-expr-runtime" \
        --optghc="-optc -I$EXAMPLES_DIR" \
        --optghc="-optc -I$EXAMPLES_DIR/golden" \
        --optghc="-optc -std=gnu2x" \
        --optghc="-optc -Wno-deprecated-declarations" \
        --optghc="-optc -Wno-attributes" \
        "$file" &>"$output_dir/haddock.log"); then
        echo "✓ $basename_file"
        return 0
    else
        echo "✗ $basename_file"
        if [[ -s "$output_dir/haddock.log" ]]; then
            echo "  Haddock error log:"
            sed 's/^/    /' "$output_dir/haddock.log"
        fi
        return 1
    fi
}

# Function to generate an index page for browsing all fixture haddocks
generate_index() {
    local index_file="$HADDOCK_OUTPUT_DIR/index.html"
    local processed_count="${#FIXTURES_TO_PROCESS[@]}"
    local skipped_count="${#FIXTURES_SKIPPED[@]}"

    cat > "$index_file" <<'EOF'
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Fixture Haddock Documentation</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            max-width: 1200px;
            margin: 40px auto;
            padding: 0 20px;
            line-height: 1.6;
            color: #333;
        }
        h1 {
            color: #2c3e50;
            border-bottom: 3px solid #3498db;
            padding-bottom: 10px;
        }
        h2 {
            color: #34495e;
            margin-top: 30px;
        }
        .stats {
            background: #ecf0f1;
            padding: 15px;
            border-radius: 5px;
            margin: 20px 0;
        }
        .fixture-list {
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
            gap: 15px;
            margin: 20px 0;
        }
        .fixture-item {
            background: #fff;
            border: 1px solid #ddd;
            border-radius: 5px;
            padding: 15px;
            transition: box-shadow 0.2s;
        }
        .fixture-item:hover {
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        .fixture-item a {
            color: #3498db;
            text-decoration: none;
            font-weight: 500;
        }
        .fixture-item a:hover {
            text-decoration: underline;
        }
        .success { color: #27ae60; }
        .error { color: #e74c3c; }
        .skipped { color: #95a5a6; }
        footer {
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #ddd;
            color: #7f8c8d;
            font-size: 0.9em;
        }
    </style>
</head>
<body>
    <h1>Fixture Haddock Documentation</h1>

    <div class="stats">
        <strong>Statistics:</strong><br>
EOF

    echo "        Total fixtures: $((processed_count + skipped_count))<br>" >> "$index_file"
    echo "        Processed: <span class=\"success\">$processed_count</span><br>" >> "$index_file"
    echo "        Skipped: <span class=\"skipped\">$skipped_count</span>" >> "$index_file"

    cat >> "$index_file" <<'EOF'
    </div>

    <h2>Fixture Documentation</h2>
    <div class="fixture-list">
EOF

    # Add links to all processed fixtures
    for file in "${FIXTURES_TO_PROCESS[@]}"; do
        local fixture_name
        fixture_name=$(basename "$file" .pp.hs)

        if [[ -f "$HADDOCK_OUTPUT_DIR/$fixture_name/index.html" ]]; then
            echo "        <div class=\"fixture-item\"><a href=\"$fixture_name/index.html\">$fixture_name</a></div>" >> "$index_file"
        else
            echo "        <div class=\"fixture-item\"><span class=\"error\">$fixture_name</span> (failed)</div>" >> "$index_file"
        fi
    done

    cat >> "$index_file" <<'EOF'
    </div>
EOF

    # Add skipped fixtures if any
    if [[ ${#FIXTURES_SKIPPED[@]} -gt 0 ]]; then
        cat >> "$index_file" <<'EOF'

    <h2>Skipped Fixtures</h2>
    <div class="fixture-list">
EOF
        for file in "${FIXTURES_SKIPPED[@]}"; do
            local fixture_name
            fixture_name=$(basename "$file" .pp.hs)
            echo "        <div class=\"fixture-item\"><span class=\"skipped\">$fixture_name</span></div>" >> "$index_file"
        done

        echo "    </div>" >> "$index_file"
    fi

    cat >> "$index_file" <<EOF

    <footer>
        Generated by compile-and-generate-haddocks-fixtures.sh<br>
        $(date)
    </footer>
</body>
</html>
EOF

    echo "📚 Generated index at: $index_file"
}

# Create persistent output directory for haddock documentation
HADDOCK_OUTPUT_DIR="$REPO_ROOT/dist-newstyle/haddock-fixtures"
mkdir -p "$HADDOCK_OUTPUT_DIR"

# Make these functions and variables available to child processes (subshells)
export -f process_fixture
export -f is_known_failure
export KNOWN_FAILURES
export HS_BINDGEN_DIR
export EXAMPLES_DIR
export HADDOCK_OUTPUT_DIR

# Collect fixtures to process
echo "Collecting fixtures..."
FIXTURES_TO_PROCESS=()
FIXTURES_SKIPPED=()

# Use find to recursively search for all .pp.hs files
while IFS= read -r -d '' file; do
    if [[ "$FORCE_ALL" == "false" ]] && is_known_failure "$file"; then
        FIXTURES_SKIPPED+=("$file")
    else
        FIXTURES_TO_PROCESS+=("$file")
    fi
done < <(find "$FIXTURES_DIR" -type f -name "*.pp.hs" -print0 | sort -z)

echo ""
echo "========================================="
echo "Fixture Compilation & Haddock Report"
echo "========================================="
echo "Total fixtures: $(find "$FIXTURES_DIR" -type f -name "*.pp.hs" | wc -l)"
echo "To process: ${#FIXTURES_TO_PROCESS[@]}"
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

if [[ ${#FIXTURES_TO_PROCESS[@]} -eq 0 ]]; then
    echo "No fixtures to process!"
    exit 0
fi

echo "Compiling and generating Haddock for ${#FIXTURES_TO_PROCESS[@]} fixtures with $JOBS parallel jobs..."
echo ""

# Process fixtures in parallel using xargs
FAILED=0
if printf '%s\n' "${FIXTURES_TO_PROCESS[@]}" | xargs -P "$JOBS" -I {} bash -c 'process_fixture "$@"' _ {}; then
    :
else
    FAILED=1
fi

echo ""
echo "========================================="
if [[ $FAILED -eq 0 ]]; then
    echo "✓ All fixtures processed successfully!"
else
    echo "✗ Some fixtures failed to process"
fi
echo "========================================="
echo ""

# Generate index page for browsing all fixture haddocks
generate_index

echo ""
echo "📚 Browse fixture documentation at:"
echo "   $HADDOCK_OUTPUT_DIR/index.html"
echo ""

if [[ $FAILED -eq 0 ]]; then
    exit 0
else
    exit 1
fi
