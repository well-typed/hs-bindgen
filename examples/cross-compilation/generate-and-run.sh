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

# ============================================================================
# Startup validation
# ============================================================================

# Check that we're inside nix develop (cross-GHC env vars are set)
if [ -z "$GHC_AARCH64_PATH" ] && [ -z "$GHC_ARM32_PATH" ]; then
    echo "# Error: Not inside nix develop environment."
    echo "#"
    echo "#   cd examples/cross-compilation"
    echo "#   nix develop"
    echo "#   ./generate-and-run.sh"
    exit 1
fi

# Check that hs-bindgen-cli is available (either on PATH or buildable via cabal)
if ! command -v hs-bindgen-cli &>/dev/null; then
    # Try building it -- cabal run will build if needed
    if ! cabal list-bin hs-bindgen-cli &>/dev/null 2>&1; then
        echo "# Warning: hs-bindgen-cli not found on PATH."
        echo "# Will attempt to build via 'cabal run hs-bindgen-cli'."
    fi
fi

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

echo "# "
echo "# hs-bindgen Cross-Compilation Example"
echo "# Target: $TARGET"
echo "# "

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

echo "# "
echo "# Phase 1: Building C libraries"
echo "# "

cd "$C_SRC_DIR"

# Clean previous builds
make clean 2>/dev/null || true

# Build native library
if [ "$TARGET" = "all" ] || [ "$TARGET" = "native" ]; then
    echo "# Building native C library"
    make CC=gcc
    mkdir -p "$C_SRC_DIR/lib-native"
    cp libarch_types.* "$C_SRC_DIR/lib-native/"
    make clean
fi

# Build cross-compiled libraries
for arch in aarch64 arm32; do
    if [ "$TARGET" = "all" ] || [ "$TARGET" = "$arch" ]; then
        configure_target "$arch"
        if command -v "$CC_CMD" &>/dev/null; then
            echo "# Building $arch C library with $CC_CMD"
            make CC="$CC_CMD"
            mkdir -p "$LIB_DIR"
            cp libarch_types.* "$LIB_DIR/"
            make clean
        else
            echo "# Warning: $arch cross-compiler not found ($CC_CMD), skipping"
        fi
    fi
done

# ============================================================================
# Phase 2: Generate Haskell bindings
# ============================================================================

echo "# "
echo "# Phase 2: Generating Haskell bindings"
echo "# "

cd "$PROJECT_ROOT"

generate_bindings() {
    local name=$1
    local target_triple=$2
    local output_dir=$3

    echo "# Generating bindings for $name"

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

if [ "$TARGET" = "all" ] || [ "$TARGET" = "native" ]; then
    generate_bindings "native" "" "$HS_PROJECT_DIR/src-native"
fi

if [ "$TARGET" = "all" ] || [ "$TARGET" = "aarch64" ]; then
    generate_bindings "aarch64" "aarch64-linux-gnu" "$HS_PROJECT_DIR/src-aarch64"
fi

if [ "$TARGET" = "all" ] || [ "$TARGET" = "arm32" ]; then
    generate_bindings "arm32" "arm-linux-gnueabihf" "$HS_PROJECT_DIR/src-arm32"
fi

# ============================================================================
# Phase 3: Build and run native Haskell executable
# ============================================================================

cd "$HS_PROJECT_DIR"

# Write cabal.project.local with library paths
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
        echo "# "
        echo "# Phase 3: Building and running native executable"
        echo "# "

        export LD_LIBRARY_PATH="$C_SRC_DIR/lib-native/:$LD_LIBRARY_PATH"

        echo "# Building native executable"
        if cabal build cross-compilation; then
            echo "# Build succeeded"
        else
            echo "# Build failed"
            exit 1
        fi

        echo "# "
        echo "# Running native executable"
        echo "# ===== Output ====="
        cabal run cross-compilation
        echo "# =================="
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
# not ship an iserv binary. The libiserv library and GHCi.Utils module are not
# in the cross-GHC's package database (only the ghci package is available,
# providing GHCi.Run, GHCi.Message, GHCi.TH, etc.). This function inlines the
# necessary source from GHC 9.6.6 (utils/iserv/ and libraries/libiserv/) and
# compiles it with the cross-GHC. The resulting binary is a target-arch
# executable that runs under QEMU.
#
# Why iservmain.c (custom C main): GHC's standard Haskell main() entry point
# does not allow customising the RTS configuration before startup. iserv
# requires a custom C main() compiled with -no-hs-main for two reasons:
#
#   1. keep_cafs = 1: Normally GHC's runtime garbage-collects CAFs (Constant
#      Applicative Forms, i.e. top-level thunks) after evaluation. But iserv
#      interprets code across multiple interactions: TH splices evaluated in
#      one round may be referenced by later splices. Without keep_cafs, the
#      RTS would GC those results, causing crashes with dangling pointers.
#
#   2. rts_opts_enabled = RtsOptsAll: allows passing RTS options (e.g. +RTS
#      -N for parallelism, -M for heap size) to iserv for debugging.
#
#   The pattern comes from GHC's iserv build:
#   https://gitlab.haskell.org/ghc/ghc/-/blob/wip/base-unit-hash/utils/iserv/cbits/iservmain.c
#
# GHC OPTIONS:
#   -fexternal-interpreter -- use iserv instead of built-in interpreter
#   -pgmi <wrapper>        -- path to the iserv wrapper script
#
# The wrapper script runs: qemu-<arch> -L <sysroot> <iserv-binary> "$@"
# GHC passes iserv's arguments (pipe file descriptors) to the wrapper.
#
# References:
#   GHC iserv source: https://gitlab.haskell.org/ghc/ghc/-/tree/master/utils/iserv
#   GHC libiserv source: https://gitlab.haskell.org/ghc/ghc/-/tree/master/libraries/libiserv
#   GHC iservmain.c: https://gitlab.haskell.org/ghc/ghc/-/blob/master/utils/iserv/cbits/iservmain.c
#   GHC external interpreter wiki: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/external-interpreter
# ============================================================================

build_iserv_for_target() {
    local target=$1
    local ghc_path=$2
    local iserv_dir="$HS_PROJECT_DIR/iserv-build-$target"
    local iserv_binary="$iserv_dir/iserv"

    # Return cached binary if already built
    if [ -x "$iserv_binary" ]; then
        echo "$iserv_binary"
        return 0
    fi

    echo "# Building iserv for $target from source..."
    mkdir -p "$iserv_dir"

    # Write the Haskell source.
    # This is a self-contained iserv that inlines:
    #   - GHCi.Utils.getGhcHandle (not exposed from the ghci package)
    #   - IServ.serv from libiserv (not available in cross-GHC)
    #
    # Source derived from GHC 9.6.6: utils/iserv/ and libraries/libiserv/
    cat > "$iserv_dir/IServMain.hs" << 'HS_EOF'
{-# LANGUAGE GADTs, ScopedTypeVariables, RankNTypes #-}
module Main (main) where

import GHCi.Run
import GHCi.TH
import GHCi.Message
import GHCi.Signals

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Binary
import Data.IORef
import Foreign.C (CInt)
import GHC.IO.Handle (Handle)
import System.Environment
import System.Exit
import System.Posix (fdToHandle, Fd(..))

-- Inlined from GHCi.Utils (module not exposed from ghci package)
getGhcHandle :: CInt -> IO Handle
getGhcHandle fd = fdToHandle $ Fd fd

-- Inlined from IServ (libiserv package not available in cross-GHC)
type MessageHook = Msg -> IO Msg

serv :: Bool -> MessageHook -> Pipe -> (forall a. IO a -> IO a) -> IO ()
serv _verbose hook pipe restore = loop
  where
    loop = do
        Msg msg <- readPipe pipe getMessage >>= hook
        discardCtrlC
        case msg of
            Shutdown -> return ()
            RunTH st q ty loc ->
                wrapRunTH $ runTH pipe st q ty loc
            RunModFinalizers st qrefs ->
                wrapRunTH $ runModFinalizerRefs pipe st qrefs
            _other -> run msg >>= reply

    reply :: forall a. (Binary a, Show a) => a -> IO ()
    reply r = do
        writePipe pipe (put r)
        loop

    wrapRunTH :: forall a. (Binary a, Show a) => IO a -> IO ()
    wrapRunTH io = do
        r <- try io
        writePipe pipe (putTHMessage RunTHDone)
        case r of
            Left e
                | Just (GHCiQException _ err) <- fromException e ->
                    reply (QFail err :: QResult a)
                | otherwise -> do
                    str <- showException e
                    reply (QException str :: QResult a)
            Right a ->
                reply (QDone a)

    showException :: SomeException -> IO String
    showException e0 = do
        r <- try $ evaluate (force (show (e0 :: SomeException)))
        case r of
            Left e  -> showException e
            Right s -> return s

    discardCtrlC = do
        r <- try $ restore $ return ()
        case r of
            Left UserInterrupt -> return () >> discardCtrlC
            Left e             -> throwIO e
            _                  -> return ()

main :: IO ()
main = do
    args <- getArgs
    (outh, inh, rest) <-
        case args of
            arg0:arg1:rest -> do
                let wfd1 = read arg0
                    rfd2 = read arg1
                inh  <- getGhcHandle rfd2
                outh <- getGhcHandle wfd1
                return (outh, inh, rest)
            _ -> do
                prog <- getProgName
                die $ prog ++ ": usage: iserv <write-fd> <read-fd> [-v]"
    let verbose = "-v" `elem` rest
    installSignalHandlers
    lo_ref <- newIORef Nothing
    let pipe = Pipe{pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}
    uninterruptibleMask $ serv verbose return pipe
HS_EOF

    # Write the C main function.
    # Sets keep_cafs=1: required for iserv to prevent GC of CAFs in
    # interpreted code (TH splices reference previously-evaluated results).
    cat > "$iserv_dir/iservmain.c" << 'C_EOF'
#include <Rts.h>
#include <HsFFI.h>

int main(int argc, char *argv[])
{
    RtsConfig conf = defaultRtsConfig;
    conf.keep_cafs = 1;
    conf.rts_opts_enabled = RtsOptsAll;
    extern StgClosure ZCMain_main_closure;
    hs_main(argc, argv, &ZCMain_main_closure, conf);
}
C_EOF

    # Compile iserv with the cross-GHC
    if "$ghc_path" -no-hs-main \
        -package ghci \
        -package deepseq \
        -package binary \
        "$iserv_dir/IServMain.hs" \
        "$iserv_dir/iservmain.c" \
        -o "$iserv_binary" 2>&1; then
        echo "# iserv built successfully: $iserv_binary"
    else
        echo "# Failed to build iserv" >&2
        return 1
    fi

    echo "$iserv_binary"
}

# ============================================================================
# Phase 4: Cross-compile and run under QEMU
# ============================================================================

# Build Haskell executable for a cross-compiled target
build_hs_cross() {
    local target=$1
    configure_target "$target"

    if [ ! -x "$GHC_PATH" ]; then
        echo "# Error: Cross-compiled GHC not found at: $GHC_PATH"
        return 1
    fi

    if [ ! -d "$LIB_DIR" ]; then
        echo "# Error: C library not found at: $LIB_DIR"
        return 1
    fi

    echo "# Building Haskell executable for $target"
    echo "#   GHC: $GHC_PATH"
    echo "#   Cabal: $CABAL_PATH"
    echo "#   Executable: $EXE_NAME"

    cd "$HS_PROJECT_DIR"

    # Derive tool paths from ghc path
    local ghc_pkg_path="${GHC_PATH%-ghc}-ghc-pkg"
    local ghc_dir
    ghc_dir="$(dirname "$GHC_PATH")"
    local hsc2hs_path

    # Find hsc2hs -- try cross-GHC's version first, fall back to native.
    #
    # hsc2hs is a build-time tool (it generates source code on the host), so
    # it does not need to be cross-compiled. The native hsc2hs works fine.
    # However, Nix's cross-GHC hsc2hs sometimes fails version checks in cabal,
    # so we fall back to the native one if the cross version doesn't work.
    #
    # See: https://downloads.haskell.org/ghc/latest/docs/users_guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs
    if [ -f "$ghc_dir/hsc2hs" ]; then
        hsc2hs_path="$ghc_dir/hsc2hs"
    else
        hsc2hs_path=$(find "$ghc_dir" -name "*hsc2hs*" -type f -executable 2>/dev/null | head -1)
    fi

    if [ -z "$hsc2hs_path" ] || ! "$hsc2hs_path" --version &>/dev/null; then
        echo "#   Note: Using native hsc2hs (cross-compiled version not working)"
        hsc2hs_path=$(command -v hsc2hs 2>/dev/null || true)
    fi

    # Set up iserv for Template Haskell cross-compilation
    local qemu_cmd_path
    qemu_cmd_path="$(command -v "$QEMU_CMD")"

    if [ -z "$qemu_cmd_path" ]; then
        echo "# Error: QEMU not found ($QEMU_CMD)"
        return 1
    fi

    # Build or retrieve cached iserv binary
    local iserv_binary
    iserv_binary=$(build_iserv_for_target "$target" "$GHC_PATH" | tail -1)

    if [ ! -x "$iserv_binary" ]; then
        echo "# Error: Failed to build iserv for $target"
        return 1
    fi

    # Create iserv wrapper script.
    # GHC's -pgmi replaces the iserv command entirely. GHC invokes the wrapper
    # with iserv's own arguments (pipe file descriptors), NOT the iserv binary
    # path. The wrapper must hardcode the iserv binary path.
    local iserv_wrapper
    iserv_wrapper="$(cd "$HS_PROJECT_DIR" && pwd)/iserv-wrapper-$target.sh"

    cat > "$iserv_wrapper" << WRAPPER_EOF
#!/usr/bin/env bash
exec $qemu_cmd_path ${QEMU_LD_PREFIX:+-L "$QEMU_LD_PREFIX"} $iserv_binary "\$@"
WRAPPER_EOF

    chmod +x "$iserv_wrapper"

    echo "#   iserv: $iserv_binary"
    echo "#   iserv wrapper: $iserv_wrapper"

    # Write cross-compilation cabal.project.local.
    # The -fexternal-interpreter and -pgmi options MUST be applied to ALL
    # packages via cabal.project.local, not just via --ghc-option. Dependencies
    # like hs-bindgen-runtime use Template Haskell and need these options too.
    cat > "$HS_PROJECT_DIR/cabal.project.local" << CABAL_EOF
-- Auto-generated by generate-and-run.sh for $target cross-compilation
package cross-compilation-example
  extra-include-dirs: $C_SRC_DIR
  extra-lib-dirs: $LIB_DIR

-- Template Haskell cross-compilation: use external interpreter under QEMU
-- This must apply to ALL packages (e.g. hs-bindgen-runtime uses TH)
package *
  ghc-options: -fexternal-interpreter -pgmi $iserv_wrapper
CABAL_EOF

    # Build with cross-compiled GHC
    if "$CABAL_PATH" build "$EXE_NAME" \
        --with-compiler="$GHC_PATH" \
        --with-ghc-pkg="$ghc_pkg_path" \
        ${hsc2hs_path:+--with-hsc2hs="$hsc2hs_path"} \
        --builddir="$BUILDDIR" \
        --extra-lib-dirs="$LIB_DIR" \
        --extra-include-dirs="$C_SRC_DIR" \
        --ghc-option="-optc-Wno-error" 2>&1 | tee /tmp/cabal-build-$target.log; then
        echo "# Build succeeded"
        return 0
    else
        echo "# Build failed (log: /tmp/cabal-build-$target.log)"
        return 1
    fi
}

# Run cross-compiled Haskell executable under QEMU
run_hs_qemu() {
    local target=$1
    configure_target "$target"

    if ! command -v "$QEMU_CMD" &>/dev/null; then
        echo "# Warning: QEMU ($QEMU_CMD) not found, cannot run $target executable"
        return 1
    fi

    cd "$HS_PROJECT_DIR"
    local exe_path
    exe_path=$(find "$BUILDDIR" -type f -name "$EXE_NAME" -executable 2>/dev/null | head -1)

    if [ -z "$exe_path" ]; then
        echo "# Error: Executable not found in $BUILDDIR"
        return 1
    fi

    echo "# Running $target executable under QEMU"
    echo "#   Executable: $exe_path"
    echo "#   QEMU: $QEMU_CMD"
    echo "#   Library: $LIB_DIR"
    echo "# ===== Output ====="

    local exit_code
    if [ -n "$QEMU_LD_PREFIX" ]; then
        LD_LIBRARY_PATH="$LIB_DIR:${LD_LIBRARY_PATH:-}" \
            "$QEMU_CMD" -L "$QEMU_LD_PREFIX" -E "LD_LIBRARY_PATH=$LIB_DIR" "$exe_path"
        exit_code=$?
    else
        echo "# Warning: QEMU_LD_PREFIX not set, system libraries may not be found"
        LD_LIBRARY_PATH="$LIB_DIR:${LD_LIBRARY_PATH:-}" \
            "$QEMU_CMD" "$exe_path"
        exit_code=$?
    fi

    echo "# =================="

    if [ $exit_code -eq 0 ]; then
        echo "# Execution succeeded"
    else
        echo "# Execution failed (exit code: $exit_code)"
    fi

    return $exit_code
}

# Run cross-compilation workflow for a target
run_cross_target() {
    local target=$1

    echo "# "
    echo "# Phase 4: Cross-compiling and running $target under QEMU"
    echo "# "

    configure_target "$target"

    if [ ! -x "$GHC_PATH" ]; then
        echo "# Cross-compiled GHC not available for $target"
        echo "# To enable: run this script inside 'nix develop'"
        return 1
    fi

    echo "# Cross-compiled GHC available for $target"

    if ! build_hs_cross "$target"; then
        echo "# Haskell build failed for $target"
        return 1
    fi

    echo "# "
    if ! run_hs_qemu "$target"; then
        echo "# QEMU execution failed for $target"
        return 1
    fi

    echo "# "
    echo "# Successfully ran $target Haskell executable under QEMU"
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

echo "# "
echo "# Done!"
echo "# "
echo "# To compare struct sizes across architectures, run:"
echo "#   ./compare-sizes.sh"
