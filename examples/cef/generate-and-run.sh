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
HS_OUTPUT_DIR="$SCRIPT_DIR/hs-project/src/"

# Create directories
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

# One library-mode run generates the bindings for all of CEF. The input headers
# are the 79 C API headers in include/capi/, plus include/cef_api_hash.h and
# include/cef_version_info.h, which declare CEF's version and hash introspection
# functions. Each of them, and each header under include/ that they include
# (such as include/internal/cef_types.h), gets its own modules, named after
# its path below include/:
#
#   include/capi/cef_app_capi.h   ->  CEF.Capi.Cef_app_capi
#   include/internal/cef_types.h  ->  CEF.Internal.Cef_types
#   include/cef_api_hash.h        ->  CEF.Cef_api_hash
#
# Add --dry-run to the call below to list every file it writes.
#
# include/internal/cef_thread_internal.h uses pid_t and pthread_t, which come
# from system headers outside include/. posix-types.yaml binds them. Without
# it, library mode skips the declarations that use them, among them the whole
# thread API of cef_thread_capi.h.
INPUT_HEADERS=(include/cef_api_hash.h include/cef_version_info.h)
for header in "$INCLUDE_DIR"/include/capi/*.h; do
    INPUT_HEADERS+=("include/capi/$(basename "$header")")
done

cabal run hs-bindgen-cli -- preprocess \
    -I "$INCLUDE_DIR" \
    --library "$INCLUDE_DIR/include" \
    --select-from-library-headers \
    --external-binding-spec "$SCRIPT_DIR/posix-types.yaml" \
    --hash-define CEF_API_VERSION 14500 \
    --hs-output-dir "$HS_OUTPUT_DIR" \
    --create-output-dirs \
    --overwrite-files \
    --module CEF \
    --omit-field-prefixes \
    "${INPUT_HEADERS[@]}"

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
