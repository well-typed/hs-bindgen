#!/usr/bin/env bash

# Exit on first error
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "# "
echo "# Configuring"
echo "# "

# shellcheck disable=SC1091
source ./configure.sh

echo "# "
echo "# Building C libraries"
echo "# "

make -C "$SCRIPT_DIR/c"

echo "# "
echo "# Generating Haskell bindings"
echo "# "

mkdir -p binding-specs

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
    -I c/ \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.macro \
    --hs-output-dir hs/manual/generated \
    --module Macro \
    macro.h

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

# Organize the generated bindings all in a single Haskell module.
cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vector \
    --hs-output-dir hs/hs-vector/generated \
    --module VectorSingleModule \
    --single-file \
    --unsafe '' \
    vector.h \
    vector_rotate.h \
    vector_length.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vector \
    --hs-output-dir hs/hs-vector/generated \
    --gen-binding-spec binding-specs/vector.yaml \
    --module Vector \
    --single-file \
    --unsafe '' \
    vector.h

sed -i.bak \
  -e '/ hsname: Vector$/s/Vector/CVector/' \
  -e '/ constructor: Vector$/s/Vector/CVector/' \
  -e '/ - vector_/s/vector_/cVector_/' \
  binding-specs/vector.yaml

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vector \
    --prescriptive-binding-spec binding-specs/vector.yaml \
    --hs-output-dir hs/hs-vector/generated \
    --module Vector \
    --single-file \
    --unsafe '' \
    vector.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vector \
    --hs-output-dir hs/hs-vector/generated \
    --external-binding-spec binding-specs/vector.yaml \
    --module Vector.Rotate \
    --single-file \
    --unsafe '' \
    vector_rotate.h

cat << EOF > binding-specs/vector-types.yaml
version:
  hs_bindgen: 0.1.0
  binding_specification: '1.0'

hsmodule: Vector.Types

ctypes:
  - headers: vector_length.h
    cname: len
    hsname: Length

hstypes:
  - hsname: Length
    instances:
      - Eq
      - HasFFIType
      - Ord
      - Show
EOF

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.vectorl \
    --hs-output-dir hs/hs-vector/generated \
    --external-binding-spec binding-specs/vector.yaml \
    --external-binding-spec binding-specs/vector-types.yaml \
    --module Vector.Length \
    --single-file \
    --unsafe '' \
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
    --select-by-decl-name '^game_state$' \
    --select-by-decl-name '^struct game_state_details$' \
    --hs-output-dir hs/hs-game/generated \
    --gen-binding-spec binding-specs/game.yaml \
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
    --external-binding-spec binding-specs/game.yaml \
    --module Game.World \
    game_world.h

cabal run --project-dir="${PROJECT_ROOT}" -- hs-bindgen-cli \
    preprocess \
    -I c \
    --create-output-dirs \
    --overwrite-files \
    --unique-id com.hs-bindgen.manual.gamep \
    --hs-output-dir hs/hs-game/generated \
    --external-binding-spec binding-specs/game.yaml \
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
