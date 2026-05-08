#!/usr/bin/env bash
#
# generate-and-run.sh - Cross-compilation workflow for hs-bindgen (aarch64)
#
# Demonstrates the full cross-compilation pipeline:
#   Phase 1: Build C library for aarch64
#   Phase 2: Generate Haskell bindings for aarch64 (preprocess mode)
#   Phase 3: Cross-compile and run preprocess-mode executable under QEMU
#   Phase 4: Cross-compile and run TH-mode executable under QEMU
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
LIBCLANG_DIR_AARCH64="${AARCH64_LIBCLANG_LIB:-}"
LIBCLANG_INC_AARCH64="${AARCH64_LIBCLANG_INCLUDE:-}"
ZLIB_DIR_AARCH64="${AARCH64_ZLIB_LIB:-}"
EXE_NAME="cross-compilation-aarch64"
EXE_NAME_TH="cross-compilation-aarch64-th"
BUILDDIR="dist-newstyle-aarch64"

# prepend_path BASE [DIR ...]: prepend non-empty DIRs to BASE, colon-joined
# (precedence-first; later args have lower precedence).
prepend_path() {
    local path="$1" entry
    shift
    for entry in "$@"; do
        [ -n "$entry" ] && path="$entry:$path"
    done
    printf '%s' "$path"
}

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
# Phase 3 + 4: Cross-compile and run under QEMU
#
# A cross-compiling GHC cannot natively execute Template Haskell splices,
# since the compiled splice code is target-architecture. GHC's solution is
# `-fexternal-interpreter`: ship the splice over a pipe to `iserv`, a
# target-arch binary that we run on the build host via QEMU user-mode
# emulation. Both phases below rely on this same iserv-on-QEMU pipeline.
#
# We build the same scaffolding once and reuse it for both phases:
#
#   * iserv binary       -- target-arch GHC external interpreter. Nix's
#                           cross-GHC does not ship one (Hadrian excludes
#                           it from cross builds), so we compile a minimal
#                           shim against `package ghci`.
#   * iserv wrapper      -- runs iserv under QEMU and injects target-arch
#                           LD_LIBRARY_PATH via QEMU's -E flag, so the RTS
#                           object linker inside iserv can dlopen target
#                           shared libraries (libgmp for both phases;
#                           libclang.so additionally for Phase 4).
#   * llvm-config stub   -- libclang-bindings's configure.ac calls
#                           llvm-config and would otherwise pick up the
#                           host one. Its AC_ARG_VAR(LLVM_CONFIG, ...) is
#                           our escape hatch.
#   * cabal.project.local -- declarative cross-build config under
#                           `package *`: extra-lib-dirs for the wrapped C
#                           lib, libgmp, libclang.so, and libz;
#                           extra-include-dirs for target zlib's headers,
#                           libclang-bindings' headers, and our c-src;
#                           and the iserv-routing
#                           `-fexternal-interpreter -pgmi <wrapper>`
#                           ghc-options so every dep's splices go through
#                           iserv. The zlib include dir is the
#                           non-obvious one -- pkg-config is not
#                           target-aware in this setup, so without it
#                           zlib's hsc preprocessing fails to find
#                           <zlib.h>.
#
# See manual/low-level/usage/cross-compilation.md (Part B + Part C) for
# the full rationale.
# ============================================================================

cd "$HS_PROJECT_DIR"

# -- Validate prerequisites --------------------------------------------------

if [ ! -x "$GHC_PATH" ]; then
    echo "  Error: cross-GHC not found at: $GHC_PATH"
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
#
# Hadrian (GHC's build system) excludes iserv from cross builds by default,
# and Nix's cross-GHC follows that default. If you built GHC from source
# with `--cross-compile-flag --enable-iserv` (or similar), iserv is already
# in your install tree and this step is a no-op.

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

# -- Create iserv wrapper -----------------------------------------------------
#
# QEMU does NOT propagate the host's environment into the emulated process
# (and the host's LD_LIBRARY_PATH would point at host-arch .so files anyway,
# which the emulated linker cannot load). The -E flag is the only way to
# inject env vars into the QEMU-side process.
#
# The LD path lists every target-arch directory iserv might dlopen from:
# the wrapped C library, target libclang (TH mode only), GHC RTS deps
# (libgmp), and zlib's bundled C source's runtime if used.

ISERV_WRAPPER="$HS_PROJECT_DIR/iserv-wrapper-aarch64.sh"

iserv_ld_path=$(prepend_path "$LIB_DIR_AARCH64" \
    "$LIBCLANG_DIR_AARCH64" "$ZLIB_DIR_AARCH64" "$AARCH64_GMP_LIB")

cat > "$ISERV_WRAPPER" << WRAPPER_EOF
#!/usr/bin/env bash
exec $qemu_cmd_path \
    ${QEMU_LD_PREFIX:+-L "$QEMU_LD_PREFIX"} \
    -E "LD_LIBRARY_PATH=$iserv_ld_path" \
    $ISERV_BINARY "\$@"
WRAPPER_EOF
chmod +x "$ISERV_WRAPPER"

# -- llvm-config stub for libclang-bindings's configure script ---------------
#
# Real `llvm-config` is a tool shipped with LLVM that exposes installation
# paths to build systems (see https://llvm.org/docs/CommandGuide/llvm-config.html).
# libclang-bindings is `build-type: Configure` and ships a configure.ac
# that finds LLVM via:
#
#     AC_PATH_PROG([LLVM_CONFIG],[llvm-config],[])
#
# We exploit that hook to point at a stub returning *target* paths.
#
# The four flags below are the subset of llvm-config's interface our
# consumers actually call:
#
#   --version    libclang-bindings configure.ac sanity check
#                (output unused beyond `$? == 0` + non-empty check;
#                see configure.ac:30-44 in the libclang-bindings tree)
#   --libdir     libclang-bindings configure.ac: LDFLAGS=-L<libdir>
#   --includedir libclang-bindings configure.ac: CPPFLAGS=-I<includedir>
#   --prefix     hs-bindgen Clang/BuiltinIncDir.hs (used to locate clang's
#                builtin headers -- only exercised when
#                BINDGEN_BUILTIN_INCLUDE_DIR != "disable")
#
# 19.1.7 happens to match LLVM in our nixos-25.05 pin, but any non-empty
# string would do.

LLVM_CONFIG_STUB="$HS_PROJECT_DIR/llvm-config-aarch64-stub.sh"
LLVM_PREFIX_AARCH64="${LIBCLANG_DIR_AARCH64%/lib}"

cat > "$LLVM_CONFIG_STUB" << STUB_EOF
#!/usr/bin/env bash
case "\$1" in
    --version)    echo "19.1.7" ;;  # value unused beyond non-empty check
    --libdir)     echo "$LIBCLANG_DIR_AARCH64" ;;
    --includedir) echo "$LIBCLANG_INC_AARCH64" ;;
    --prefix)     echo "$LLVM_PREFIX_AARCH64" ;;
esac
STUB_EOF
chmod +x "$LLVM_CONFIG_STUB"

# -- Configure cabal for cross-compilation -----------------------------------
#
# Everything declarative goes here so the same `cabal build` command works
# for both phases. Phase-4-only stanzas (target zlib include dir,
# libclang-bindings include dirs) are harmless for Phase 3: the preprocess
# executable's transitive closure does not include zlib or
# libclang-bindings, so the stanzas are simply not exercised there.

cat > "$HS_PROJECT_DIR/cabal.project.local" << CABAL_EOF
-- Auto-generated by generate-and-run.sh for aarch64 cross-compilation.
-- Do not edit by hand: the absolute Nix store paths below are baked in at
-- script-generation time and become stale after any \`nix develop\` change.
-- Re-run ./generate-and-run.sh to regenerate.

package *
  -- -fexternal-interpreter routes TH splices through iserv.
  ghc-options: -fexternal-interpreter -pgmi $ISERV_WRAPPER
  extra-lib-dirs: $LIB_DIR_AARCH64
  ${AARCH64_GMP_LIB:+extra-lib-dirs: $AARCH64_GMP_LIB}
  ${LIBCLANG_DIR_AARCH64:+extra-lib-dirs: $LIBCLANG_DIR_AARCH64}
  ${ZLIB_DIR_AARCH64:+extra-lib-dirs: $ZLIB_DIR_AARCH64}
  -- zlib's hsc files \`#include <zlib.h>\`. With the default
  -- \`+pkg-config\` flag, cabal asks pkg-config for the include path,
  -- but pkg-config is not target-aware in our cross setup, so the
  -- preprocessing fails with \`zlib.h: No such file or directory\`.
  -- Adding the target zlib's include dir here lets the cross-CC find
  -- it without overriding cabal flags.
  ${AARCH64_ZLIB_INCLUDE:+extra-include-dirs: $AARCH64_ZLIB_INCLUDE}

package cross-compilation-example
  extra-include-dirs: $C_SRC_DIR

package libclang-bindings
  ${LIBCLANG_INC_AARCH64:+extra-include-dirs: $LIBCLANG_INC_AARCH64}
CABAL_EOF

# -- Helpers shared by Phases 3 and 4 ----------------------------------------
#
# Phase 3 and Phase 4 differ only in the executable being built and (for
# Phase 4) two env vars set on the cabal invocation. Everything else is
# identical, so we share it here.
#
# --with-hsc2hs is required because Nix's cross-GHC ships hsc2hs under a
# target-prefixed name (aarch64-unknown-linux-gnu-hsc2hs) that cabal does
# not discover automatically.

ghc_pkg_path="${GHC_PATH%-ghc}-ghc-pkg"
hsc2hs_path="${GHC_PATH%-ghc}-hsc2hs"

cabal_args=(
    --with-compiler="$GHC_PATH"
    --with-ghc-pkg="$ghc_pkg_path"
    --builddir="$BUILDDIR"
)
[ -n "$hsc2hs_path" ] && cabal_args+=(--with-hsc2hs="$hsc2hs_path")

aarch64_cabal_build() {
    "$CABAL_PATH" build "$1" "${cabal_args[@]}"
}

aarch64_exe_path() {
    "$CABAL_PATH" list-bin "$1" "${cabal_args[@]}"
}

# qemu_run EXE LD_PATH: run EXE under QEMU with target-arch LD_LIBRARY_PATH.
# As with the iserv wrapper, host LD_LIBRARY_PATH is *not* propagated;
# QEMU's -E flag is the only path in.
qemu_run() {
    local exe="$1" ld_path="$2"
    if [ -z "$QEMU_LD_PREFIX" ]; then
        echo "  Warning: QEMU_LD_PREFIX not set, system libraries may not be found"
        "$QEMU_CMD" "$exe"
    else
        "$QEMU_CMD" -L "$QEMU_LD_PREFIX" -E "LD_LIBRARY_PATH=$ld_path" "$exe"
    fi
}

# -- Phase 3: preprocess-mode executable -------------------------------------
#
# The bindings already live in src-aarch64/, generated by hs-bindgen-cli on
# the host in Phase 2. The cross-GHC just compiles them. iserv handles the
# small TH splices internal to hs-bindgen-runtime (Storable derivations
# etc.), but no libclang is invoked at TH time.

echo "==> Phase 3: Cross-compiling and running aarch64 under QEMU"
echo "  Building Haskell executable for aarch64"
aarch64_cabal_build "$EXE_NAME"

echo "  Running aarch64 executable under QEMU:"
qemu_run "$(aarch64_exe_path "$EXE_NAME")" "$LIB_DIR_AARCH64"
echo "  Successfully ran aarch64 Haskell executable under QEMU"

# -- Phase 4: TH-mode executable ---------------------------------------------
#
# The splice (\`withHsBindgen ... \$ hashInclude "arch_types.h"\`) runs in
# iserv-on-QEMU. iserv loads the cross-built hs-bindgen object code, which
# dlopens target libclang.so (resolved via the iserv wrapper's
# LD_LIBRARY_PATH set above) and parses the header. The resulting Haskell
# declarations are inlined into the user's Main.o and linked against the
# wrapped C library (\`extra-libraries: arch_types\` in the .cabal).
#
# Two env vars feed information into the splice:
#   * LLVM_CONFIG   - so libclang-bindings's configure script (run during
#                     cross-build of hs-bindgen) finds target LLVM paths.
#                     Without it, configure picks up the host llvm-config and
#                     fails with "Cannot find libclang headers".
#   * BINDGEN_BUILTIN_INCLUDE_DIR=disable - hygiene, not strictly required
#                     for arch_types.h (it has no system #includes). With
#                     it unset, the splice falls through to host-clang
#                     discovery and silently uses host builtin includes,
#                     which would be wrong for headers that #include
#                     <stdint.h> etc. \`disable\` is a documented value;
#                     see hs-bindgen/src-internal/HsBindgen/Clang/BuiltinIncDir.hs.

echo "==> Phase 4: Cross-compiling and running aarch64-th under QEMU (TH mode)"
echo "  Building Haskell TH executable for aarch64"

LLVM_CONFIG="$LLVM_CONFIG_STUB" \
BINDGEN_BUILTIN_INCLUDE_DIR=disable \
    aarch64_cabal_build "$EXE_NAME_TH"

th_run_ld_path=$(prepend_path "$LIB_DIR_AARCH64" \
    "$LIBCLANG_DIR_AARCH64" "$ZLIB_DIR_AARCH64" "$AARCH64_GMP_LIB")

echo "  Running aarch64-th executable under QEMU:"
qemu_run "$(aarch64_exe_path "$EXE_NAME_TH")" "$th_run_ld_path"
echo "  Successfully ran aarch64-th Haskell executable under QEMU"

echo "==> Done!"
