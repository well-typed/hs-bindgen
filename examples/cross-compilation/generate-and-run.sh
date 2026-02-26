#!/usr/bin/env bash
#
# generate-and-run.sh - Cross-compilation workflow for hs-bindgen (aarch64)
#
# Demonstrates the full cross-compilation pipeline:
#   Phase 1: Build C library for aarch64
#   Phase 2: Generate Haskell bindings for aarch64
#   Phase 3: Cross-compile and run Haskell executable under QEMU
#
# Requires: nix develop (provides cross-GHC, QEMU, sysroots)
#
# Usage:
#   ./generate-and-run.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
C_SRC_DIR="$SCRIPT_DIR/c-src"
HS_PROJECT_DIR="$SCRIPT_DIR/hs-project"

# ============================================================================
# Target configuration (aarch64)
# ============================================================================

TARGET_TRIPLE="aarch64-linux-gnu"
GHC_PATH="$GHC_AARCH64_PATH"
CABAL_PATH="$CABAL_AARCH64_PATH"
QEMU_CMD="qemu-aarch64"
QEMU_LD_PREFIX="${QEMU_AARCH64_LD_PREFIX:-}"
LIB_DIR_AARCH64="$C_SRC_DIR/lib-aarch64"
EXE_NAME="cross-compilation-aarch64"
BUILDDIR="dist-newstyle-aarch64"

echo "==> hs-bindgen Cross-Compilation: aarch64"

# ============================================================================
# Startup validation
# ============================================================================

if [ -z "$GHC_AARCH64_PATH" ] || [ -z "$AARCH64_CC" ]; then
    echo "# Error: Not inside nix develop environment."
    echo "#"
    echo "#   cd examples/cross-compilation"
    echo "#   nix develop"
    echo "#   ./generate-and-run.sh"
    exit 1
fi

# ============================================================================
# Phase 1: Build C library
# ============================================================================

echo "==> Phase 1: Building C library for aarch64"

cd "$C_SRC_DIR"

# Build aarch64 C library using the cross-compiling Clang wrapper from Nix
# (llvmPackages.stdenv.cc). The wrapper has target sysroot headers, linker
# paths, and --target baked in — just override CC, no extra flags needed.
echo "  Building aarch64 C library (cross-clang)"
make clean >/dev/null 2>&1 || true
make CC="$AARCH64_CC" >/dev/null
mkdir -p "$LIB_DIR_AARCH64"
cp libarch_types.* "$LIB_DIR_AARCH64/"

make clean >/dev/null 2>&1

# ============================================================================
# Phase 2: Generate Haskell bindings
# ============================================================================

echo "==> Phase 2: Generating Haskell bindings"

cd "$PROJECT_ROOT"

generate_bindings() {
    local target_triple=$1
    local output_dir=$2

    local clang_opts=()
    if [ -n "$target_triple" ]; then
        clang_opts+=(--clang-option="--target=$target_triple")

        # When using --target, libclang resolves headers directly (not via the
        # Nix CC wrapper), so system headers may not be found. Pass the target
        # sysroot's include path explicitly. The example header doesn't need
        # this (no system #includes), but this makes the example robust for
        # headers that do.
        if [ -n "${AARCH64_SYSROOT:-}" ]; then
            clang_opts+=(--clang-option="-isystem${AARCH64_SYSROOT}/include")
        fi
    fi

    BINDGEN_EXTRA_CLANG_ARGS="" \
    BINDGEN_BUILTIN_INCLUDE_DIR="" \
    cabal run hs-bindgen-cli -- \
        preprocess \
        "${clang_opts[@]}" \
        -I "$C_SRC_DIR" \
        --hs-output-dir "$output_dir" \
        --create-output-dirs \
        --overwrite-files \
        --module ArchTypes.Generated \
        "$C_SRC_DIR/arch_types.h"
}

# Generate aarch64 bindings
echo "  Generating bindings for aarch64"
generate_bindings "$TARGET_TRIPLE" "$HS_PROJECT_DIR/src-aarch64"

# ============================================================================
# Phase 3: Cross-compile and run under QEMU
#
# iserv: External interpreter for Template Haskell cross-compilation
#
# See manual/LowLevel/Usage/08-CrossCompilation.md for detailed explanation.
# Brief: iserv evaluates TH splices under QEMU for the target architecture.
# We build it from source because Nix's cross-GHC does not ship an iserv
# binary (Hadrian explicitly excludes it from cross builds). If you built
# GHC from source with Hadrian's default settings, iserv is already included
# in the installation and this step is unnecessary.
# ============================================================================

echo "==> Phase 3: Cross-compiling and running aarch64 under QEMU"

cd "$HS_PROJECT_DIR"

# -- Validate prerequisites --------------------------------------------------

if [ ! -x "$GHC_PATH" ]; then
    echo "  Error: Cross-compiled GHC not found at: $GHC_PATH"
    exit 1
fi

if [ ! -d "$LIB_DIR_AARCH64" ]; then
    echo "  Error: C library not found at: $LIB_DIR_AARCH64"
    exit 1
fi

qemu_cmd_path="$(command -v "$QEMU_CMD")" || true
if [ -z "$qemu_cmd_path" ]; then
    echo "  Error: QEMU not found ($QEMU_CMD)"
    exit 1
fi

# -- Build iserv from source -------------------------------------------------

ISERV_DIR="$HS_PROJECT_DIR/iserv-build-aarch64"
ISERV_BINARY="$ISERV_DIR/iserv"

if [ ! -x "$ISERV_BINARY" ]; then
    echo "  Building iserv for aarch64 from source..."
    mkdir -p "$ISERV_DIR"

    cat > "$ISERV_DIR/IServ.hs" << 'HS_EOF'
module Main (main) where
import GHCi.Server (defaultServer)
main :: IO ()
main = defaultServer
HS_EOF

    "$GHC_PATH" \
        -package ghci \
        -rtsopts=all \
        ${AARCH64_GMP_LIB:+-L"$AARCH64_GMP_LIB"} \
        "$ISERV_DIR/IServ.hs" \
        -o "$ISERV_BINARY"

    echo "  iserv built: $ISERV_BINARY"
fi

# -- Create iserv wrapper script ----------------------------------------------

ISERV_WRAPPER="$HS_PROJECT_DIR/iserv-wrapper-aarch64.sh"

cat > "$ISERV_WRAPPER" << WRAPPER_EOF
#!/usr/bin/env bash
exec $qemu_cmd_path ${QEMU_LD_PREFIX:+-L "$QEMU_LD_PREFIX"} $ISERV_BINARY "\$@"
WRAPPER_EOF

chmod +x "$ISERV_WRAPPER"

# -- Configure cabal for cross-compilation ------------------------------------

ghc_pkg_path="${GHC_PATH%-ghc}-ghc-pkg"
hsc2hs_path="${GHC_PATH%-ghc}-hsc2hs"

cat > "$HS_PROJECT_DIR/cabal.project.local" << CABAL_EOF
-- Auto-generated by generate-and-run.sh for aarch64 cross-compilation
package cross-compilation-example
  extra-include-dirs: $C_SRC_DIR
  extra-lib-dirs: $LIB_DIR_AARCH64

package *
  ghc-options: -fexternal-interpreter -pgmi $ISERV_WRAPPER
  ${AARCH64_GMP_LIB:+extra-lib-dirs: $AARCH64_GMP_LIB}
CABAL_EOF

# -- Build cross-compiled executable ------------------------------------------
#
# Note: --with-hsc2hs is required because Nix's cross-GHC ships hsc2hs under
# a target-prefixed name (e.g. aarch64-unknown-linux-gnu-hsc2hs) that cabal
# does not discover automatically. The example includes a .hsc file
# (app/ArchSizes.hsc) to demonstrate this requirement.

echo "  Building Haskell executable for aarch64"

if ! "$CABAL_PATH" build "$EXE_NAME" \
    --with-compiler="$GHC_PATH" \
    --with-ghc-pkg="$ghc_pkg_path" \
    ${hsc2hs_path:+--with-hsc2hs="$hsc2hs_path"} \
    --builddir="$BUILDDIR" \
    --extra-lib-dirs="$LIB_DIR_AARCH64" \
    ${AARCH64_GMP_LIB:+--extra-lib-dirs="$AARCH64_GMP_LIB"} \
    --extra-include-dirs="$C_SRC_DIR" \
    --ghc-option="-optc-Wno-error"; then
    echo "  Build failed"
    exit 1
fi

# -- Run under QEMU -----------------------------------------------------------

exe_path=$(find "$BUILDDIR" -type f -name "$EXE_NAME" -executable 2>/dev/null | head -1)

if [ -z "$exe_path" ]; then
    echo "  Error: Executable not found in $BUILDDIR"
    exit 1
fi

echo "  Running aarch64 executable under QEMU:"

if [ -n "$QEMU_LD_PREFIX" ]; then
    LD_LIBRARY_PATH="$LIB_DIR_AARCH64:${LD_LIBRARY_PATH:-}" \
        "$QEMU_CMD" -L "$QEMU_LD_PREFIX" -E "LD_LIBRARY_PATH=$LIB_DIR_AARCH64" "$exe_path"
else
    echo "  Warning: QEMU_LD_PREFIX not set, system libraries may not be found"
    LD_LIBRARY_PATH="$LIB_DIR_AARCH64:${LD_LIBRARY_PATH:-}" \
        "$QEMU_CMD" "$exe_path"
fi

echo "  Successfully ran aarch64 Haskell executable under QEMU"

echo "==> Done!"
