#!/usr/bin/env bash
#
# Build a dependency-light libgit2 from the vendored submodule.
#
# cmake and a C toolchain are pulled from nixpkgs (this repo is a Nix flake and
# the hs-bindgen dev shell does not ship cmake), so no system packages are
# required. HTTPS/SSH are disabled and regex/zlib/http-parser use libgit2's
# builtin backends, which removes the openssl/libssh2/pcre/zlib dependencies and
# keeps the build self-contained.
#
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SRC="$SCRIPT_DIR/libgit2"
BUILD="$SRC/build"

if [ ! -f "$SRC/CMakeLists.txt" ]; then
  echo "libgit2 submodule missing; run:" >&2
  echo "  git submodule update --init $SRC" >&2
  exit 1
fi

# Run a command with cmake + a C toolchain from nixpkgs on PATH.
run() {
  nix shell nixpkgs#cmake nixpkgs#gnumake nixpkgs#gcc --command "$@"
}

echo "==> Configuring libgit2 (no HTTPS/SSH; builtin regex/zlib/http-parser/sha)"
run cmake -S "$SRC" -B "$BUILD" \
  -DCMAKE_BUILD_TYPE=Release \
  -DBUILD_SHARED_LIBS=ON \
  -DBUILD_TESTS=OFF -DBUILD_CLI=OFF -DBUILD_EXAMPLES=OFF \
  -DUSE_SSH=OFF -DUSE_HTTPS=OFF -DUSE_NTLMCLIENT=OFF \
  -DUSE_HTTP_PARSER=builtin -DREGEX_BACKEND=builtin \
  -DUSE_BUNDLED_ZLIB=ON \
  -DUSE_SHA1=CollisionDetection -DUSE_SHA256=Builtin

echo "==> Building"
run cmake --build "$BUILD" -j"$(nproc)"

SO="$(find "$BUILD" \( -name 'libgit2.so*' -o -name 'libgit2.dylib' \) 2>/dev/null | head -1)"
if [ -z "$SO" ]; then
  echo "ERROR: libgit2 shared library not found under $BUILD" >&2
  exit 1
fi
echo "==> Built: $SO"
