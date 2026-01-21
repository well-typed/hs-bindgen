#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
INCLUDE_DIR="$SCRIPT_DIR/rpm/include"
RPMIO_DIR="$SCRIPT_DIR/rpm/_build/rpmio/"
RPMBUILD_DIR="$SCRIPT_DIR/rpm/_build/build/"
BINDING_SPEC_DIR="$SCRIPT_DIR/binding-specs"
HS_OUTPUT_DIR="$SCRIPT_DIR/hs-project/src/"

echo $SCRIPT_DIR

# Create directories
mkdir -p "$BINDING_SPEC_DIR"
mkdir -p "$HS_OUTPUT_DIR"

echo "# "
echo "# Building RPM library from source"
echo "# "

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
echo "# Generating Haskell bindings in dependency order"
echo "# "

cd "$PROJECT_ROOT"

# Helper function to generate bindings with optional external binding specs
# Usage: generate_bindings HEADER MODULE_NAME [--no-binding-spec] [external specs...]
generate_bindings() {
    local HEADER="$1"
    local MODULE_NAME="$2"
    shift 2

    local BINDING_SPEC_FILE="$BINDING_SPEC_DIR/${HEADER%.h}.yaml"
    local EXTERNAL_SPECS=("$@")

    echo "Generating bindings for $HEADER -> $MODULE_NAME"

    local CMD=(
        cabal run hs-bindgen-cli -- preprocess
        -I "$INCLUDE_DIR"
        --hs-output-dir "$HS_OUTPUT_DIR"
        --create-output-dirs
        --overwrite-files
        --module "$MODULE_NAME"
        --parse-all
        --select-from-main-headers
        --enable-program-slicing
        --gen-binding-spec "$BINDING_SPEC_FILE"
      )

    # Add external binding specs if any
    for spec in "${EXTERNAL_SPECS[@]}"; do
        if [ -f "$spec" ]; then
            CMD+=(--external-binding-spec "$spec")
        fi
    done

    CMD+=("rpm/$HEADER")

    "${CMD[@]}"
}

# Generate bindings in dependency order

# 1. rpmtypes.h (no dependencies)
generate_bindings "rpmtypes.h" "RPM.Types"

# 2. rpmsw.h (no dependencies)
generate_bindings "rpmsw.h" "RPM.Sw"

# 3. rpmutil.h (no dependencies)
generate_bindings "rpmutil.h" "RPM.Util"

# 4. rpmtag.h (depends on rpmtypes)
generate_bindings "rpmtag.h" "RPM.Tag" \
    "$BINDING_SPEC_DIR/rpmtypes.yaml"

# 5. argv.h (depends on rpmtypes)
generate_bindings "argv.h" "RPM.Argv" \
    "$BINDING_SPEC_DIR/rpmtypes.yaml"

# 6. rpmprob.h (depends on rpmtypes)
generate_bindings "rpmprob.h" "RPM.Prob" \
    "$BINDING_SPEC_DIR/rpmtypes.yaml"

# 7. rpmio.h (depends on rpmtypes, rpmsw)
generate_bindings "rpmio.h" "RPM.IO" \
    "$BINDING_SPEC_DIR/rpmtypes.yaml" \
    "$BINDING_SPEC_DIR/rpmsw.yaml"

# 8. rpmtd.h (depends on rpmtag, argv)
generate_bindings "rpmtd.h" "RPM.Td" \
    "$BINDING_SPEC_DIR/rpmtag.yaml" \
    "$BINDING_SPEC_DIR/argv.yaml"

# 9. rpmps.h (depends on rpmtypes, rpmprob)
generate_bindings "rpmps.h" "RPM.Ps" \
    "$BINDING_SPEC_DIR/rpmtypes.yaml" \
    "$BINDING_SPEC_DIR/rpmprob.yaml"

# 10. header.h (depends on rpmio, rpmtypes, rpmtd, rpmutil)
generate_bindings "header.h" "RPM.Header" \
    "$BINDING_SPEC_DIR/rpmio.yaml" \
    "$BINDING_SPEC_DIR/rpmtypes.yaml" \
    "$BINDING_SPEC_DIR/rpmutil.yaml" \
    "$BINDING_SPEC_DIR/rpmtd.yaml"

# 11. rpmds.h (depends on rpmtypes, rpmutil, rpmps)
generate_bindings "rpmds.h" "RPM.Ds" \
    "$BINDING_SPEC_DIR/rpmtypes.yaml" \
    "$BINDING_SPEC_DIR/rpmutil.yaml" \
    "$BINDING_SPEC_DIR/rpmps.yaml"

# 12. rpmver.h (depends on rpmtypes, rpmds)
generate_bindings "rpmver.h" "RPM.Ver" \
    "$BINDING_SPEC_DIR/rpmtypes.yaml" \
    "$BINDING_SPEC_DIR/rpmds.yaml"

# 13. rpmlib.h (depends on rpmio, header, rpmtag, rpmds, rpmver)
generate_bindings "rpmlib.h" "RPM.Lib" \
    "$BINDING_SPEC_DIR/rpmio.yaml" \
    "$BINDING_SPEC_DIR/header.yaml" \
    "$BINDING_SPEC_DIR/rpmtag.yaml" \
    "$BINDING_SPEC_DIR/rpmds.yaml" \
    "$BINDING_SPEC_DIR/rpmver.yaml" \

echo "# "
echo "# Creating cabal.project.local"
echo "# "

cat > "$SCRIPT_DIR/hs-project/cabal.project.local" <<EOF
package c-rpm
    extra-include-dirs:
        $INCLUDE_DIR
    extra-lib-dirs:
        $RPMIO_DIR
        $RPMBUILD_DIR
EOF

cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "
echo "Running the project"

cd "$SCRIPT_DIR/hs-project"
LD_LIBRARY_PATH="$RPMIO_DIR:$RPMBUILD_DIR:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

cabal build
cabal run c-rpm
