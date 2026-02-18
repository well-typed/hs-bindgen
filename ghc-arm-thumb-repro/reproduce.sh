#!/usr/bin/env bash
#
# Minimal reproducer for GHC ARM Thumb interworking bug (ghc#21991)
#
# Prerequisites:
#   cd ghc-arm-thumb-repro
#   nix develop    # provides cross-GHC 9.8.4, QEMU, cross-GCC
#
# What this does:
#   1. Builds iserv (GHC's external interpreter) for ARM32
#   2. Cross-compiles a Haskell project that uses TH addForeignSource
#      with C code calling strlen
#   3. The build crashes with SIGILL during TH evaluation because the
#      runtime linker generates BL instead of BLX for Thumb targets
#
set -e

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$DIR"

# Check environment
if [ -z "$GHC" ]; then
    echo "Error: Not inside nix develop environment."
    echo ""
    echo "  cd ghc-arm-thumb-repro"
    echo "  nix develop"
    echo "  ./reproduce.sh"
    exit 1
fi

echo "GHC: $GHC"
echo ""

# ============================================================================
# Step 1: Build iserv for ARM32
# ============================================================================

echo "==> Building iserv for ARM32..."
mkdir -p iserv-build

cat > iserv-build/IServ.hs << 'EOF'
module Main (main) where
import GHCi.Server (defaultServer)
main :: IO ()
main = defaultServer
EOF

ISERV="$DIR/iserv-build/iserv"
if [ ! -x "$ISERV" ]; then
    "$GHC" -package ghci -fkeep-cafs -rtsopts=all \
        iserv-build/IServ.hs -o "$ISERV"
    echo "  Built: $ISERV"
else
    echo "  Using cached: $ISERV"
fi

# ============================================================================
# Step 2: Create iserv wrapper (runs iserv under QEMU)
# ============================================================================

QEMU_CMD="$(command -v qemu-arm)"
WRAPPER="$DIR/iserv-wrapper.sh"

cat > "$WRAPPER" << WRAPPER_EOF
#!/usr/bin/env bash
exec $QEMU_CMD ${QEMU_ARM_LD_PREFIX:+-L "$QEMU_ARM_LD_PREFIX"} $ISERV "\$@"
WRAPPER_EOF
chmod +x "$WRAPPER"

# ============================================================================
# Step 3: Cross-compile — this triggers the bug
# ============================================================================

echo ""
echo "==> Cross-compiling with TH..."
echo "    This will crash with SIGILL during Template Haskell evaluation."
echo "    The crash occurs because the runtime linker generates BL (ARM-to-ARM)"
echo "    instead of BLX (ARM-to-Thumb) when calling strlen from glibc."
echo ""

GHC_PKG="${GHC}-pkg"

"$CABAL" build exe:repro \
    --with-compiler="$GHC" \
    --with-ghc-pkg="$GHC_PKG" \
    --builddir=dist-arm32 \
    --ghc-options="-fexternal-interpreter -pgmi $WRAPPER"
