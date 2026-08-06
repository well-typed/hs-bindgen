#!/usr/bin/env bash
#
# Build libsodium from the fetched source tree into a local install prefix.
#
# autoconf/automake/libtool/m4 and a C toolchain are pulled from nixpkgs (the
# hs-bindgen dev shell does not ship them), so no system packages are required.
# We build from a git checkout, so ./autogen.sh runs first to produce ./configure
# (it no-ops without -s). Installing into build-prefix/ gives a clean include/ +
# lib/ layout, including the generated sodium/version.h, to hand to cabal.
#
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SRC="$SCRIPT_DIR/libsodium"
PREFIX="$SRC/build-prefix"

if [ ! -f "$SRC/autogen.sh" ]; then
  echo "libsodium source missing; run ./generate-and-run.sh (it fetches the pinned source)" >&2
  exit 1
fi

# autogen.sh downloads config.guess/config.sub from savannah unless told not to;
# keep the build self-contained and offline (automake --add-missing supplies them).
export DO_NOT_UPDATE_CONFIG_SCRIPTS=1

# Run a command with autotools + a C toolchain from nixpkgs on PATH.
run() {
  nix shell nixpkgs#autoconf nixpkgs#automake nixpkgs#libtool nixpkgs#m4 \
            nixpkgs#gnumake nixpkgs#gcc --command "$@"
}

cd "$SRC"

echo "==> autogen (generating ./configure)"
run ./autogen.sh -s

echo "==> configure (prefix $PREFIX)"
run ./configure --prefix="$PREFIX" --disable-dependency-tracking

echo "==> build"
run make -j"$(nproc)"

echo "==> install"
run make install

SO="$(find "$PREFIX/lib" \( -name 'libsodium.so*' -o -name 'libsodium.dylib' \) 2>/dev/null | head -1)"
if [ -z "$SO" ]; then
  echo "ERROR: libsodium shared library not found under $PREFIX/lib" >&2
  exit 1
fi
echo "==> Built: $SO"
