#!/usr/bin/env bash

# Exit on first error
set -e

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
echo "# Basic examples"
echo "# "

mkdir -p hs/manual/generated

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --hs-output-dir hs/manual/generated \
    --module Example \
    manual_examples.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/manual/generated \
    --module Structs \
    structs.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/manual/generated \
    --module Globals \
    globals.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/manual/generated \
    --module Arrays \
    arrays.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/manual/generated \
    --module FunctionPointers \
    function_pointers.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c/ \
    --hs-output-dir hs/manual/generated \
    --module Complex \
    hsb_complex_test.h

echo "# "
echo "# External bindings: vector example"
echo "# "

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/hs-vector/generated \
    --gen-binding-spec external/vector.yaml \
    --module Vector \
    vector.h

mkdir -p hs/hs-vector/generated/Vector

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/hs-vector/generated \
    --external-binding-spec external/vector.yaml \
    --module Vector.Rotate \
    vector_rotate.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/hs-vector/generated \
    --external-binding-spec external/vector.yaml \
    --module Vector.Length \
    vector_length.h

echo "# "
echo "# External bindings: game example"
echo "# "

mkdir -p hs/hs-game/generated/Game

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --select-by-header-path 'game_internal\.h$' \
    --hs-output-dir hs/hs-game/generated \
    --gen-binding-spec external/game.yaml \
    --module Game.State \
    game_world.h \
    game_player.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/hs-game/generated \
    --external-binding-spec external/game.yaml \
    --module Game.World \
    game_world.h

cabal run hs-bindgen-cli -- \
    preprocess \
    -I c \
    --hs-output-dir hs/hs-game/generated \
    --external-binding-spec external/game.yaml \
    --module Game.Player \
    game_player.h
