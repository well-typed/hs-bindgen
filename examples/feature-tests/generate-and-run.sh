#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
export PROJECT_ROOT

# Platform detection. See manual/configure.sh for the canonical pattern; this
# script is invoked on all three CI runners (Linux, macOS, Windows).
case "$(uname -s)" in
    Linux)              PLATFORM=linux   ;;
    Darwin)             PLATFORM=darwin  ;;
    MINGW*|MSYS*|CYGWIN*) PLATFORM=windows ;;
    *)
        echo "Unsupported platform: $(uname -s)" >&2
        exit 1
        ;;
esac

(
    echo "# "
    echo "# Building C library"
    echo "# "

    cd "$SCRIPT_DIR/c"
    make CC="${CC:-clang}"
)

C_DIR=$(realpath c)
echo $C_DIR

echo "# "
echo "# Generating Haskell bindings"
echo "# "

GENERATED_DIR=hs-project/src-generated/Generated
if [ -d "$GENERATED_DIR" ]; then
  rm -r "$GENERATED_DIR"
fi

echo "## Callbacks"

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Callbacks.Arrays.KnownSize \
    --omit-field-prefixes \
    callbacks/arrays/known_size.h

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Callbacks.Arrays.UnknownSize \
    --omit-field-prefixes \
    callbacks/arrays/unknown_size.h

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Callbacks.Basic \
    --omit-field-prefixes \
    callbacks/basic.h

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Callbacks.Structs \
    --omit-field-prefixes \
    callbacks/structs.h

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Callbacks.Unions \
    --omit-field-prefixes \
    callbacks/unions.h

echo "## Pointer manipulation"

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.PointerManipulation \
    --omit-field-prefixes \
    pointer_manipulation.h

echo "## Types"

echo "### Anonymous"

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Types.Anonymous \
    types/anonymous.h

echo "### Bit-fields"

cabal run --project-file="${PROJECT_ROOT}/cabal.project" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --hs-output-dir hs-project/src-generated \
    --unique-id feature-tests.well-typed.com \
    --create-output-dirs \
    --overwrite-files \
    --module Generated.Types.Bitfields \
    types/bitfields.h

echo "# "
echo "# Updating cabal.project.local"
echo "# "

# On Windows, Cabal expects Windows-style paths in extra-include-dirs /
# extra-lib-dirs.  Convert with cygpath; on Unix the path is already correct.
if [ "$PLATFORM" = "windows" ]; then
    CABAL_C_DIR=$(cygpath -aw "$C_DIR")
else
    CABAL_C_DIR="$C_DIR"
fi

LINE=$(cat <<-EOF
package feature-tests
    extra-include-dirs: $CABAL_C_DIR
    extra-lib-dirs: $CABAL_C_DIR
EOF
)

grep -qxF "$LINE" "$SCRIPT_DIR/hs-project/cabal.project.local" || echo "$LINE" >> "$SCRIPT_DIR/hs-project/cabal.project.local"
cat "$SCRIPT_DIR/hs-project/cabal.project.local"

echo "# "
echo "# Done!"
echo "# "

(
    echo "# "
    echo "Running the project"
    echo "# "

    # Make the freshly-built shared library discoverable by the test executable.
    # Each platform uses a different mechanism:
    #   - Linux:   LD_LIBRARY_PATH
    #   - macOS:   DYLD_LIBRARY_PATH (matches manual/configure.sh)
    #   - Windows: PATH (Windows-style), because DLLs are resolved via PATH
    case "$PLATFORM" in
        linux)
            LD_LIBRARY_PATH="$C_DIR:${LD_LIBRARY_PATH:-}"
            export LD_LIBRARY_PATH
            echo "LD_LIBRARY_PATH: $LD_LIBRARY_PATH"
            ;;
        darwin)
            DYLD_LIBRARY_PATH="$C_DIR:${DYLD_LIBRARY_PATH:-}"
            export DYLD_LIBRARY_PATH
            echo "DYLD_LIBRARY_PATH: $DYLD_LIBRARY_PATH"
            ;;
        windows)
            C_DIR_WINDOWS=$(cygpath -aw "$C_DIR")
            PATH="$C_DIR_WINDOWS:$PATH"
            export PATH
            echo "PATH (prepended with): $C_DIR_WINDOWS"
            ;;
    esac

    cd "hs-project"
    cabal build
    cabal run
)
