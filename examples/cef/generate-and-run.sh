#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# CEF version to download (pinned for reproducibility)
CEF_VERSION="145.0.28+g51162e8+chromium-145.0.7632.160"
CEF_PLATFORM="linux64_minimal"
# The CEF binary distribution uses '+' in the version, which is URL-encoded as
# '%2B' on the Spotify CDN.
CEF_DIR_NAME="cef_binary_${CEF_VERSION}_${CEF_PLATFORM}"
CEF_ARCHIVE="${CEF_DIR_NAME}.tar.bz2"
CEF_URL="https://cef-builds.spotifycdn.com/${CEF_ARCHIVE//+/%2B}"

CEF_ROOT="$SCRIPT_DIR/$CEF_DIR_NAME"
BINDING_SPEC_DIR="$SCRIPT_DIR/binding-specs"
HS_OUTPUT_DIR="$SCRIPT_DIR/hs-project/src/"

# Create directories
mkdir -p "$BINDING_SPEC_DIR"
mkdir -p "$HS_OUTPUT_DIR"

echo "# "
echo "# Downloading CEF binary distribution"
echo "# "

if [ ! -d "$CEF_ROOT" ]; then
    if [ ! -f "$SCRIPT_DIR/$CEF_ARCHIVE" ]; then
        echo "Downloading $CEF_ARCHIVE (~287MB)..."
        curl -L -o "$SCRIPT_DIR/$CEF_ARCHIVE" "$CEF_URL"
    else
        echo "Archive already downloaded."
    fi

    echo "Extracting..."
    tar -xjf "$SCRIPT_DIR/$CEF_ARCHIVE" -C "$SCRIPT_DIR"
    rm -f "$SCRIPT_DIR/$CEF_ARCHIVE"
    echo "Extracted to $CEF_ROOT"
else
    echo "CEF distribution already present at $CEF_ROOT"
fi

# CEF headers use #include "include/capi/cef_base_capi.h" (relative to CEF
# root), so -I must point to the CEF distribution root directory.
INCLUDE_DIR="$CEF_ROOT"
LIB_DIR="$CEF_ROOT/Release"

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cd "$PROJECT_ROOT"

# Non-capi headers (under include/, not include/capi/) declare CEF's
# version/hash introspection functions. They are independent of the capi
# headers, so we generate them with regular preprocess calls and their own
# binding spec chain.
generate_non_capi_binding() {
    local HEADER="$1"
    local MODULE_NAME="$2"
    shift 2

    local HEADER_BASENAME
    HEADER_BASENAME="$(basename "${HEADER%.h}")"
    local BINDING_SPEC_FILE="$BINDING_SPEC_DIR/${HEADER_BASENAME}.yaml"
    local EXTERNAL_SPECS=("$@")

    echo "Generating bindings for $HEADER -> $MODULE_NAME"

    local CMD=(
        cabal run hs-bindgen-cli -- preprocess
        -I "$INCLUDE_DIR"
        --hash-define CEF_API_VERSION 14500
        --hs-output-dir "$HS_OUTPUT_DIR"
        --create-output-dirs
        --overwrite-files
        --module "$MODULE_NAME"
        --select-from-main-headers
        --enable-program-slicing
        --omit-field-prefixes
        --gen-binding-spec "$BINDING_SPEC_FILE"
      )

    for spec in "${EXTERNAL_SPECS[@]}"; do
        if [ -f "$spec" ]; then
            CMD+=(--external-binding-spec "$spec")
        fi
    done

    CMD+=("$HEADER")

    "${CMD[@]}"
}

# cef_api_hash.h: declares cef_api_hash() and cef_api_version()
generate_non_capi_binding "include/cef_api_hash.h" "CEF.Cef_api_hash"

# cef_version_info.h: declares cef_version_info() and cef_version_info_all()
generate_non_capi_binding "include/cef_version_info.h" "CEF.Cef_version_info" \
    "$BINDING_SPEC_DIR/cef_api_hash.yaml"

# All 79 capi headers in one call. preprocess-library walks the include graph
# of cef_app_capi.h (the top-level header that transitively includes every
# other capi header), assigns each sub-header its own Haskell module, and
# chains binding specs automatically in dependency order.
cabal run hs-bindgen-cli -- preprocess-library \
    -I "$INCLUDE_DIR" \
    --hash-define CEF_API_VERSION 14500 \
    --hs-output-dir "$HS_OUTPUT_DIR" \
    --create-output-dirs \
    --overwrite-files \
    --module CEF \
    --library-root "$CEF_ROOT/include/capi" \
    --omit-field-prefixes \
    "include/capi/cef_app_capi.h"

echo "# "
echo "# Generating cabal.project.paths"
echo "# "

cat > "$SCRIPT_DIR/hs-project/cabal.project.paths" <<EOF
package cef
    extra-include-dirs:
        $INCLUDE_DIR
    extra-lib-dirs:
        $LIB_DIR
EOF
cat "$SCRIPT_DIR/hs-project/cabal.project.paths"

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"

cd "$SCRIPT_DIR/hs-project"
LD_LIBRARY_PATH="$LIB_DIR:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

cabal build cef-bin

# CEF uses dladdr() to find libcef.so's directory and looks for resource files
# (icudtl.dat, pak files, locales/) there. The binary distribution puts these
# in Resources/ while libcef.so is in Release/, so we symlink them across.
for f in "$CEF_ROOT/Resources/"*; do
    ln -sf "$f" "$CEF_ROOT/Release/"
done

cabal run cef-bin
