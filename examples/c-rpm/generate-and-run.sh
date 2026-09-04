#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
INCLUDE_DIR="$SCRIPT_DIR/rpm/include"
RPMIO_DIR="$SCRIPT_DIR/rpm/_build/rpmio/"
RPMBUILD_DIR="$SCRIPT_DIR/rpm/_build/build/"
HS_OUTPUT_DIR="$SCRIPT_DIR/hs-project/src/"

# RPM Package Manager, pinned to a git tag so the example is reproducible. We
# fetch it on demand rather than as a git submodule: a submodule is cloned by
# cabal for every project that depends on hs-bindgen via
# source-repository-package, even though only hs-bindgen-runtime is needed.
RPM_REPO="https://github.com/rpm-software-management/rpm.git"
RPM_TAG="rpm-6.1.0-rc1"

echo $SCRIPT_DIR

# Create directories
mkdir -p "$HS_OUTPUT_DIR"

echo "# "
echo "# Building RPM library from source"
echo "# "

if [ ! -d "$SCRIPT_DIR/rpm/.git" ]; then
    # --filter=blob:none keeps the download small (blobs are fetched on demand)
    # while still allowing checkout of the pinned tag; a shallow clone could not
    # check out an arbitrary older tag.
    git clone --filter=blob:none "$RPM_REPO" "$SCRIPT_DIR/rpm"
fi
git -C "$SCRIPT_DIR/rpm" checkout --quiet "$RPM_TAG"
# rpm carries its own submodules that its build needs.
git -C "$SCRIPT_DIR/rpm" submodule update --init --recursive

cd "$SCRIPT_DIR/rpm"

# Create build directory if it doesn't exist
if [ ! -d "_build" ]; then
    echo "Creating _build directory..."
    mkdir _build
fi

cd _build

# Configure with cmake if not already configured
if [ ! -f "CMakeCache.txt" ]; then
    echo "Configuring with cmake..."
    cmake ${RPM_CMAKE_FLAGS} ..
fi

# Build RPM
echo "Building RPM (this may take a while)..."
make -j$(nproc)

echo "# "
echo "# Generating Haskell bindings"
echo "# "

cd "$PROJECT_ROOT"

# preprocess-library walks the include graph of rpmlib.h (which transitively
# includes all 13 RPM public headers), assigns each sub-header its own Haskell
# module, and chains binding specs automatically in dependency order.
cabal run hs-bindgen-cli -- preprocess-library \
    -I "$INCLUDE_DIR" \
    --hs-output-dir "$HS_OUTPUT_DIR" \
    --create-output-dirs \
    --overwrite-files \
    --module RPM \
    --library-root "$INCLUDE_DIR/rpm" \
    rpm/rpmlib.h

echo "# "
echo "# Generating cabal.project.paths"
echo "# "

cat > "$SCRIPT_DIR/hs-project/cabal.project.paths" <<EOF
package c-rpm
    extra-include-dirs:
        $INCLUDE_DIR
    extra-lib-dirs:
        $RPMIO_DIR
        $RPMBUILD_DIR
EOF
cat "$SCRIPT_DIR/hs-project/cabal.project.paths"

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"

cd "$SCRIPT_DIR/hs-project"
LD_LIBRARY_PATH="$RPMIO_DIR:$RPMBUILD_DIR:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

cabal build
cabal run c-rpm
