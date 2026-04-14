#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# There's a quirk with Apple and Windows assembler and LLVM IR that do not
# accept Unicode characters. There's SUPPORTS_UNICODE flag that allows Unicode
# characters and we only enable that for non-MacOS and non-LLVM backend
# compilation
#
# Check inside manual_examples.{c,h} for where this macro flag is used.
#
if [[ "$(uname -s)" == "Linux" && "${LLVM_BACKEND}" != "1" ]]; then
    echo "Setting SUPPORTS_UNICODE in BINDGEN_EXTRA_CLANG_ARGS"
    export BINDGEN_EXTRA_CLANG_ARGS="-DSUPPORTS_UNICODE ${BINDGEN_EXTRA_CLANG_ARGS:-}"
else
    echo "Not setting SUPPORTS_UNICODE (not Linux or LLVM backend enabled)"
fi

echo "# "
echo "# Building C libraries"
echo "# "

make -C "$SCRIPT_DIR/c"

export LD_LIBRARY_PATH="$SCRIPT_DIR/c:${LD_LIBRARY_PATH:-}"
if [[ "$(uname -s)" == "Darwin" ]]; then
    export DYLD_LIBRARY_PATH="$SCRIPT_DIR/c:${DYLD_LIBRARY_PATH:-}"
fi

if [[ -f "$SCRIPT_DIR/hs/cabal.project.local" ]]; then
    echo "# "
    echo "# Using existing cabal.project.local"
    echo "# "
else
    echo "# "
    echo "# Generating cabal.project.local"
    echo "# "

    SUPPORTS_UNICODE_STANZA=""
    if [[ "$(uname -s)" == "Linux" && "${LLVM_BACKEND}" != "1" ]]; then
        SUPPORTS_UNICODE_STANZA="package manual
  ghc-options:
    -optc-DSUPPORTS_UNICODE
    -DSUPPORTS_UNICODE

"
    fi

    cat >"$SCRIPT_DIR/hs/cabal.project.local" <<EOF
${SUPPORTS_UNICODE_STANZA}package manual
  extra-include-dirs:
      $SCRIPT_DIR/c
  extra-lib-dirs:
      $SCRIPT_DIR/c

package hs-game
  extra-include-dirs:
      $SCRIPT_DIR/c
  extra-lib-dirs:
      $SCRIPT_DIR/c

package hs-vector
  extra-include-dirs:
      $SCRIPT_DIR/c
  extra-lib-dirs:
      $SCRIPT_DIR/c
EOF
fi

echo "# "
echo "# Generating Haskell bindings"
echo "# "

mkdir -p external

echo "# "
echo "# Basic examples"
echo "# "

mkdir -p hs/manual/generated

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c/ \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.example \
    --hs-output-dir hs/manual/generated \
    --module Example \
    manual_examples.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.globals \
    --hs-output-dir hs/manual/generated \
    --module Globals \
    globals.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.arrays \
    --hs-output-dir hs/manual/generated \
    --module Arrays \
    arrays.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.funptrs \
    --hs-output-dir hs/manual/generated \
    --module FunctionPointers \
    function_pointers.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c/ \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.complex \
    --hs-output-dir hs/manual/generated \
    --module Complex \
    hsb_complex_test.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c/ \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.callbacks \
    --hs-output-dir hs/manual/generated \
    --module Callbacks \
    callbacks.h

echo "# "
echo "# Pointer manipulation API"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c/ \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.pointermanipulation \
    --hs-output-dir hs/manual/generated \
    --module PointerManipulation \
    pointer_manipulation.h

echo "# "
echo "# Structs"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.structs \
    --hs-output-dir hs/manual/generated \
    --module Structs \
    structs.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.structs \
    --hs-output-dir hs/manual/generated \
    --module Structs.Nesting \
    structs/nesting.h

echo "# "
echo "# Unions"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.unions \
    --hs-output-dir hs/manual/generated \
    --module Unions \
    unions.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.unions \
    --hs-output-dir hs/manual/generated \
    --module Unions.Nesting \
    unions/nesting.h

echo "# "
echo "# Unprefixed field names"
echo "# "

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.omitfieldprefixes \
    --hs-output-dir hs/manual/generated \
    --module OmitFieldPrefixes \
    --omit-field-prefixes \
    omit_field_prefixes.h

echo "# "
echo "# External bindings: vector example"
echo "# "

mkdir -p hs/hs-vector/generated

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vector \
    --hs-output-dir hs/hs-vector/generated \
    --gen-binding-spec external/vector.yaml \
    --module Vector \
    vector.h

mkdir -p hs/hs-vector/generated/Vector

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vector \
    --hs-output-dir hs/hs-vector/generated \
    --external-binding-spec external/vector.yaml \
    --module Vector.Rotate \
    vector_rotate.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vectorl \
    --hs-output-dir hs/hs-vector/generated \
    --external-binding-spec external/vector.yaml \
    --module Vector.Length \
    vector_length.h

echo "# "
echo "# External bindings: game example"
echo "# "

mkdir -p hs/hs-game/generated/Game

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.game \
    --select-by-header-path 'game_internal\.h$' \
    --hs-output-dir hs/hs-game/generated \
    --gen-binding-spec external/game.yaml \
    --module Game.State \
    game_world.h \
    game_player.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.gamew \
    --hs-output-dir hs/hs-game/generated \
    --external-binding-spec external/game.yaml \
    --module Game.World \
    game_world.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.gamep \
    --hs-output-dir hs/hs-game/generated \
    --external-binding-spec external/game.yaml \
    --module Game.Player \
    game_player.h

echo "# "
echo "# Building and running the manual"
echo "# "

(
    cd "$SCRIPT_DIR/hs"
    cabal build all
    cabal run manual
)
