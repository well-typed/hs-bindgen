#!/usr/bin/env bash
#
# generate-and-run.sh - Cross-compilation workflow for hs-bindgen
#
# Demonstrates the full cross-compilation pipeline:
#   Phase 1: Build C libraries (native + cross-compiled)
#   Phase 2: Generate Haskell bindings (hs-bindgen --target=<triple>)
#   Phase 3: Build and run native Haskell executable
#   Phase 4: Cross-compile and run under QEMU (iserv + cross-GHC)
#
# Requires: nix develop (provides cross-GHC, QEMU, sysroots)
#
# Usage:
#   ./generate-and-run.sh         # Run all targets
#   ./generate-and-run.sh native  # Run only native
#   ./generate-and-run.sh aarch64 # Run only aarch64
#   ./generate-and-run.sh arm32   # Run only 32-bit ARM
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
C_SRC_DIR="$SCRIPT_DIR/c-src"
HS_PROJECT_DIR="$SCRIPT_DIR/hs-project"

# Parse command line arguments
TARGET="${1:-all}"

case "$TARGET" in
    all|native|aarch64|arm32)
        ;;
    *)
        echo "Usage: $0 [all|native|aarch64|arm32]"
        exit 1
        ;;
esac

echo "==> hs-bindgen Cross-Compilation: $TARGET"

# ============================================================================
# Startup validation
# ============================================================================

# Check that we're inside nix develop (cross-GHC env vars are set)
if [ "$TARGET" != "native" ] && [ -z "$GHC_AARCH64_PATH" ] && [ -z "$GHC_ARM32_PATH" ]; then
    echo "# Error: Not inside nix develop environment."
    echo "#"
    echo "#   cd examples/cross-compilation"
    echo "#   nix develop"
    echo "#   ./generate-and-run.sh"
    exit 1
fi


# ============================================================================
# Target configuration
#
# Sets all target-specific variables in one place.
# ============================================================================

# Configure variables for a given target.
# Sets: TARGET_TRIPLE, GHC_PATH, CABAL_PATH, QEMU_CMD, QEMU_LD_PREFIX,
#        LIB_DIR, EXE_NAME, BUILDDIR, CC_CMD
configure_target() {
    local target=$1

    case "$target" in
        native)
            TARGET_TRIPLE=""
            GHC_PATH=""
            CABAL_PATH=""
            QEMU_CMD=""
            QEMU_LD_PREFIX=""
            LIB_DIR="$C_SRC_DIR/lib-native"
            EXE_NAME="cross-compilation"
            BUILDDIR="dist-newstyle"
            CC_CMD="gcc"
            ;;
        aarch64)
            TARGET_TRIPLE="aarch64-linux-gnu"
            GHC_PATH="$GHC_AARCH64_PATH"
            CABAL_PATH="$CABAL_AARCH64_PATH"
            QEMU_CMD="qemu-aarch64"
            QEMU_LD_PREFIX="${QEMU_AARCH64_LD_PREFIX:-}"
            LIB_DIR="$C_SRC_DIR/lib-aarch64"
            EXE_NAME="cross-compilation-aarch64"
            BUILDDIR="dist-newstyle-aarch64"
            CC_CMD="${AARCH64_CC:-aarch64-linux-gnu-gcc}"
            ;;
        arm32)
            TARGET_TRIPLE="arm-linux-gnueabihf"
            GHC_PATH="$GHC_ARM32_PATH"
            CABAL_PATH="$CABAL_ARM32_PATH"
            QEMU_CMD="qemu-arm"
            QEMU_LD_PREFIX="${QEMU_ARM_LD_PREFIX:-}"
            LIB_DIR="$C_SRC_DIR/lib-arm32"
            EXE_NAME="cross-compilation-arm32"
            BUILDDIR="dist-newstyle-arm32"
            CC_CMD="${ARM32_CC:-armv7l-unknown-linux-gnueabihf-gcc}"
            ;;
        *)
            echo "# Error: Unknown target: $target"
            return 1
            ;;
    esac
}

# ============================================================================
# Phase 1: Build C libraries
# ============================================================================

echo "==> Phase 1: Building C libraries"

cd "$C_SRC_DIR"
make clean >/dev/null 2>&1 || true

for target in native aarch64 arm32; do
    if [ "$TARGET" = "all" ] || [ "$TARGET" = "$target" ]; then
        configure_target "$target"

        if command -v "$CC_CMD" &>/dev/null; then
            echo "  Building $target C library ($CC_CMD)"
            cd "$C_SRC_DIR"
            make CC="$CC_CMD" >/dev/null
            mkdir -p "$LIB_DIR"
            cp libarch_types.* "$LIB_DIR/"
            make clean >/dev/null 2>&1
        else
            echo "  Warning: Compiler not found ($CC_CMD), skipping $target"
        fi
    fi
done

# ============================================================================
# Phase 2: Generate Haskell bindings
# ============================================================================

echo "==> Phase 2: Generating Haskell bindings"

cd "$PROJECT_ROOT"

generate_bindings() {
    local name=$1
    local target_triple=$2
    local output_dir=$3

    local clang_opts=()
    if [ -n "$target_triple" ]; then
        clang_opts+=(--clang-option="--target=$target_triple")
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

for target in native aarch64 arm32; do
    if [ "$TARGET" = "all" ] || [ "$TARGET" = "$target" ]; then
        configure_target "$target"
        src_dir="$HS_PROJECT_DIR/src-$target"
        echo "  Generating bindings for $target"
        generate_bindings "$target" "$TARGET_TRIPLE" "$src_dir"
    fi
done

# ============================================================================
# Phase 3: Build and run native Haskell executable
#
# Note: Native is handled separately from cross-targets because it doesn't
# require the cross-compilation infrastructure (iserv, QEMU, cross-GHC).
# ============================================================================

cd "$HS_PROJECT_DIR"

cat > cabal.project.local << EOF
-- Auto-generated by generate-and-run.sh
package cross-compilation-example
  extra-include-dirs: $C_SRC_DIR
  extra-lib-dirs:
    $C_SRC_DIR/lib-native/
    $C_SRC_DIR/lib-aarch64/
    $C_SRC_DIR/lib-arm32/
EOF

if [ "$TARGET" = "all" ] || [ "$TARGET" = "native" ]; then
    if [ -d "$C_SRC_DIR/lib-native" ]; then
        echo "==> Phase 3: Building and running native executable"
        export LD_LIBRARY_PATH="$C_SRC_DIR/lib-native/:$LD_LIBRARY_PATH"

        echo "  Building native executable"
        cabal build cross-compilation >/dev/null || { echo "  Build failed"; exit 1; }

        echo "  Running native executable:"
        cabal run cross-compilation
    fi
fi

# ============================================================================
# iserv: External interpreter for Template Haskell cross-compilation
#
# What: iserv is GHC's "external interpreter" -- a separate process that
# evaluates Template Haskell splices on behalf of the compiler.
#
# Why Needed: During cross-compilation, GHC runs on the host (x86_64) but
# produces code for the target (e.g. aarch64). Template Haskell splices must
# execute at compile time, but the compiled code is for the target architecture
# and cannot run on the host. The solution is -fexternal-interpreter: GHC
# communicates with an iserv process over pipes. The iserv binary is a
# target-architecture executable that runs under QEMU.
#
# How it works:
#   1. GHC encounters a TH splice
#   2. GHC sends the splice to iserv via pipe (using the GHCi.Message protocol)
#   3. iserv (running under QEMU) evaluates the splice
#   4. iserv sends the result back to GHC via pipe
#   5. GHC incorporates the result into the compiled output
#
# Why build from source: Nix's cross-GHC (pkgsCross.*.buildPackages.ghc) does
# not ship an iserv binary. However, the ghci package is available in the
# cross-GHC's package database and provides GHCi.Server.defaultServer, which
# is the entire iserv implementation. Building iserv from source is just
# compiling a 4-line module:
#
#   module Main (main) where
#   import GHCi.Server (defaultServer)
#   main :: IO ()
#   main = defaultServer
#
# We compile with two additional GHC flags to configure the RTS:
#
#   -fkeep-cafs: Prevents the RTS from garbage-collecting CAFs (Constant
#     Applicative Forms -- top-level thunks). Without this flag, the RTS would
#     GC those results, causing crashes from dangling pointers.
#
#   -rtsopts=all: Allows passing RTS options (e.g. +RTS -M for heap size)
#     to iserv for debugging.
#
# GHC OPTIONS:
#   -fexternal-interpreter -- use iserv instead of built-in interpreter
#   -pgmi <wrapper>        -- path to the iserv wrapper script
#
# The wrapper script runs: qemu-<arch> -L <sysroot> <iserv-binary> "$@"
# GHC passes iserv's arguments (pipe file descriptors) to the wrapper.
#
# References:
#   GHC GHCI iserv: https://gitlab.haskell.org/ghc/ghc/-/blob/master/docs/users_guide/ghci.rst
#   GHC external interpreter wiki: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/external-interpreter
# ============================================================================

build_iserv_for_target() {
    local target=$1
    local ghc_path=$2
    local iserv_dir="$HS_PROJECT_DIR/iserv-build-$target"
    local iserv_binary="$iserv_dir/iserv"

    if [ -x "$iserv_binary" ]; then
        echo "$iserv_binary"
        return 0
    fi

    echo "  Building iserv for $target from source..."
    mkdir -p "$iserv_dir"

    cat > "$iserv_dir/IServ.hs" << 'HS_EOF'
module Main (main) where
import GHCi.Server (defaultServer)
main :: IO ()
main = defaultServer
HS_EOF

    if "$ghc_path" \
        -package ghci \
        -fkeep-cafs \
        -rtsopts=all \
        "$iserv_dir/IServ.hs" \
        -o "$iserv_binary"; then
        echo "  iserv built: $iserv_binary"
    else
        echo "  Failed to build iserv" >&2
        return 1
    fi

    echo "$iserv_binary"
}

# ============================================================================
# Phase 4: Cross-compile and run under QEMU
# ============================================================================

build_hs_cross() {
    local target=$1
    configure_target "$target"

    if [ ! -x "$GHC_PATH" ]; then
        echo "  Error: Cross-compiled GHC not found at: $GHC_PATH"
        return 1
    fi

    if [ ! -d "$LIB_DIR" ]; then
        echo "  Error: C library not found at: $LIB_DIR"
        return 1
    fi

    echo "  Building Haskell executable for $target"
    cd "$HS_PROJECT_DIR"

    local ghc_pkg_path="${GHC_PATH%-ghc}-ghc-pkg"
    local hsc2hs_path="${GHC_PATH%-ghc}-hsc2hs"

    local qemu_cmd_path
    qemu_cmd_path="$(command -v "$QEMU_CMD")"

    if [ -z "$qemu_cmd_path" ]; then
        echo "  Error: QEMU not found ($QEMU_CMD)"
        return 1
    fi

    local iserv_binary
    iserv_binary=$(build_iserv_for_target "$target" "$GHC_PATH" | tail -1)

    if [ ! -x "$iserv_binary" ]; then
        echo "  Error: Failed to build iserv for $target"
        return 1
    fi

    local iserv_wrapper
    iserv_wrapper="$(cd "$HS_PROJECT_DIR" && pwd)/iserv-wrapper-$target.sh"

    cat > "$iserv_wrapper" << WRAPPER_EOF
#!/usr/bin/env bash
exec $qemu_cmd_path ${QEMU_LD_PREFIX:+-L "$QEMU_LD_PREFIX"} $iserv_binary "\$@"
WRAPPER_EOF

    chmod +x "$iserv_wrapper"

    cat > "$HS_PROJECT_DIR/cabal.project.local" << CABAL_EOF
-- Auto-generated by generate-and-run.sh for $target cross-compilation
package cross-compilation-example
  extra-include-dirs: $C_SRC_DIR
  extra-lib-dirs: $LIB_DIR

package *
  ghc-options: -fexternal-interpreter -pgmi $iserv_wrapper
CABAL_EOF

    if "$CABAL_PATH" build "$EXE_NAME" \
        --with-compiler="$GHC_PATH" \
        --with-ghc-pkg="$ghc_pkg_path" \
        ${hsc2hs_path:+--with-hsc2hs="$hsc2hs_path"} \
        --builddir="$BUILDDIR" \
        --extra-lib-dirs="$LIB_DIR" \
        --extra-include-dirs="$C_SRC_DIR" \
        --ghc-option="-optc-Wno-error"; then
        return 0
    else
        echo "  Build failed (see /tmp/cabal-build-$target.log)"
        return 1
    fi
}

run_hs_qemu() {
    local target=$1
    configure_target "$target"

    if ! command -v "$QEMU_CMD" &>/dev/null; then
        echo "  Warning: QEMU ($QEMU_CMD) not found, cannot run $target executable"
        return 1
    fi

    cd "$HS_PROJECT_DIR"
    local exe_path
    exe_path=$(find "$BUILDDIR" -type f -name "$EXE_NAME" -executable 2>/dev/null | head -1)

    if [ -z "$exe_path" ]; then
        echo "  Error: Executable not found in $BUILDDIR"
        return 1
    fi

    echo "  Running $target executable under QEMU:"
    local exit_code
    if [ -n "$QEMU_LD_PREFIX" ]; then
        LD_LIBRARY_PATH="$LIB_DIR:${LD_LIBRARY_PATH:-}" \
            "$QEMU_CMD" -L "$QEMU_LD_PREFIX" -E "LD_LIBRARY_PATH=$LIB_DIR" "$exe_path"
        exit_code=$?
    else
        echo "  Warning: QEMU_LD_PREFIX not set, system libraries may not be found"
        LD_LIBRARY_PATH="$LIB_DIR:${LD_LIBRARY_PATH:-}" \
            "$QEMU_CMD" "$exe_path"
        exit_code=$?
    fi

    if [ $exit_code -eq 0 ]; then
        echo "  Execution succeeded"
    else
        echo "  Execution failed (exit code: $exit_code)"
    fi

    return $exit_code
}

run_cross_target() {
    local target=$1

    echo "==> Phase 4: Cross-compiling and running $target under QEMU"

    configure_target "$target"

    if [ ! -x "$GHC_PATH" ]; then
        echo "  Cross-compiled GHC not available for $target"
        echo "  To enable: run this script inside 'nix develop'"
        return 1
    fi

    if ! build_hs_cross "$target"; then
        return 1
    fi

    if ! run_hs_qemu "$target"; then
        return 1
    fi

    echo "  Successfully ran $target Haskell executable under QEMU"
    return 0
}

# Run cross-compilation for each requested target
for arch in aarch64 arm32; do
    if [ "$TARGET" = "all" ] || [ "$TARGET" = "$arch" ]; then
        configure_target "$arch"
        if [ -d "$LIB_DIR" ]; then
            run_cross_target "$arch"
        fi
    fi
done

echo "==> Done!"
echo "  To compare struct sizes across architectures, run: ./compare-sizes.sh"
