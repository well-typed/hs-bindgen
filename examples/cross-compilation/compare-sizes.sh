#!/usr/bin/env bash
#
# compare-sizes.sh - Compare struct sizes across different target architectures
#
# This script extracts and compares the sizeOf, alignment, and field offsets
# from generated Haskell bindings to demonstrate architecture differences.
#
# Usage:
#   ./compare-sizes.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Function to extract size info from generated Haskell file
extract_sizes() {
    local file=$1
    local struct_name=$2

    if [ ! -f "$file" ]; then
        echo "NOT_FOUND"
        return
    fi

    # Extract sizeOf value using sed for more reliable parsing
    # The format is: sizeOf = \_ -> (24 :: Int)
    local size=$(sed -n '/instance F.Storable '"$struct_name"'/,/poke/p' "$file" | \
                 grep 'sizeOf' | grep -oP '\(\K\d+(?= :: Int\))' | head -1)

    # Extract alignment value using sed for more reliable parsing
    # The format is: alignment = \_ -> (8 :: Int)
    local align=$(sed -n '/instance F.Storable '"$struct_name"'/,/poke/p' "$file" | \
                  grep 'alignment' | grep -oP '\(\K\d+(?= :: Int\))' | head -1)

    echo "${size:-?}|${align:-?}"
}

# Function to extract field offset
extract_offset() {
    local file=$1
    local field_name=$2

    if [ ! -f "$file" ]; then
        echo "?"
        return
    fi

    # Look for offset# = \_ -> \_ -> N pattern
    # The instance declaration pattern includes the field name
    local offset=$(grep -A5 'instance HsBindgen.Runtime.HasCField.HasCField.*"'"$field_name"'"' "$file" | \
                   grep "offset#" | grep -oE '[0-9]+$' | head -1)
    echo "${offset:-?}"
}

print_header() {
    echo ""
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════════════════════════════${NC}"
}

print_subheader() {
    echo ""
    echo -e "${CYAN}─── $1 ───${NC}"
}

# Check if bindings have been generated
NATIVE_FILE="$SCRIPT_DIR/hs-project/src-native/ArchTypes/Generated.hs"
AARCH64_FILE="$SCRIPT_DIR/hs-project/src-aarch64/ArchTypes/Generated.hs"

MISSING_FILES=0
if [ ! -f "$NATIVE_FILE" ]; then
    echo -e "${YELLOW}Warning: Native bindings not found. Run ./generate-and-run.sh first.${NC}"
    MISSING_FILES=1
fi
if [ ! -f "$AARCH64_FILE" ]; then
    echo -e "${YELLOW}Warning: aarch64 bindings not found. Run ./generate-and-run.sh first.${NC}"
    MISSING_FILES=1
fi

if [ $MISSING_FILES -eq 1 ]; then
    echo ""
    echo "Please run ./generate-and-run.sh all first to generate bindings for all targets."
    exit 1
fi

print_header "Cross-Compilation Size Comparison"

# Compare ArchInfo struct
print_subheader "struct ArchInfo"

echo ""
printf "%-20s %10s %10s %10s\n" "Platform" "sizeOf" "alignment" "Data Model"
printf "%-20s %10s %10s %10s\n" "────────────────────" "──────────" "──────────" "──────────"

NATIVE_INFO=$(extract_sizes "$NATIVE_FILE" "ArchInfo")
NATIVE_SIZE=$(echo "$NATIVE_INFO" | cut -d'|' -f1)
NATIVE_ALIGN=$(echo "$NATIVE_INFO" | cut -d'|' -f2)
printf "%-20s %10s %10s %10s\n" "Native (x86_64)" "$NATIVE_SIZE" "$NATIVE_ALIGN" "LP64"

AARCH64_INFO=$(extract_sizes "$AARCH64_FILE" "ArchInfo")
AARCH64_SIZE=$(echo "$AARCH64_INFO" | cut -d'|' -f1)
AARCH64_ALIGN=$(echo "$AARCH64_INFO" | cut -d'|' -f2)
printf "%-20s %10s %10s %10s\n" "aarch64-linux-gnu" "$AARCH64_SIZE" "$AARCH64_ALIGN" "LP64"

echo ""
echo "Field offsets in ArchInfo:"
printf "%-20s %8s %8s %8s %8s\n" "Platform" "value" "data" "count" "flags"
printf "%-20s %8s %8s %8s %8s\n" "────────────────────" "────────" "────────" "────────" "────────"

printf "%-20s %8s %8s %8s %8s\n" "Native (x86_64)" \
    "$(extract_offset "$NATIVE_FILE" "archInfo_value")" \
    "$(extract_offset "$NATIVE_FILE" "archInfo_data")" \
    "$(extract_offset "$NATIVE_FILE" "archInfo_count")" \
    "$(extract_offset "$NATIVE_FILE" "archInfo_flags")"

printf "%-20s %8s %8s %8s %8s\n" "aarch64-linux-gnu" \
    "$(extract_offset "$AARCH64_FILE" "archInfo_value")" \
    "$(extract_offset "$AARCH64_FILE" "archInfo_data")" \
    "$(extract_offset "$AARCH64_FILE" "archInfo_count")" \
    "$(extract_offset "$AARCH64_FILE" "archInfo_flags")"

# Compare PointerArray struct
print_subheader "struct PointerArray"

echo ""
printf "%-20s %10s %10s\n" "Platform" "sizeOf" "alignment"
printf "%-20s %10s %10s\n" "────────────────────" "──────────" "──────────"

NATIVE_INFO=$(extract_sizes "$NATIVE_FILE" "PointerArray")
printf "%-20s %10s %10s\n" "Native (x86_64)" $(echo "$NATIVE_INFO" | tr '|' ' ')

AARCH64_INFO=$(extract_sizes "$AARCH64_FILE" "PointerArray")
printf "%-20s %10s %10s\n" "aarch64-linux-gnu" $(echo "$AARCH64_INFO" | tr '|' ' ')

# Compare NestedStruct
print_subheader "struct NestedStruct"

echo ""
printf "%-20s %10s %10s\n" "Platform" "sizeOf" "alignment"
printf "%-20s %10s %10s\n" "────────────────────" "──────────" "──────────"

NATIVE_INFO=$(extract_sizes "$NATIVE_FILE" "NestedStruct")
printf "%-20s %10s %10s\n" "Native (x86_64)" $(echo "$NATIVE_INFO" | tr '|' ' ')

AARCH64_INFO=$(extract_sizes "$AARCH64_FILE" "NestedStruct")
printf "%-20s %10s %10s\n" "aarch64-linux-gnu" $(echo "$AARCH64_INFO" | tr '|' ' ')
