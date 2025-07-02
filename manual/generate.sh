#!/usr/bin/env bash

PROJECT_ROOT=..

# Exit on first error
set -e

echo "# "
echo "# Basic examples"
echo "# "

mkdir -p hs/manual/generated

cabal run hs-bindgen-cli -- \
  preprocess \
    -I ${PROJECT_ROOT}/hs-bindgen/examples/golden \
    -o hs/manual/generated/Example.hs \
    --module Example \
    manual_examples.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/manual/generated/Structs.hs \
    --module Structs \
    structs.h

echo "# "
echo "# External bindings: vector example"
echo "# "

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-vector/generated/Vector.hs \
    --gen-binding-spec external/vector.yaml \
    --module Vector \
    vector.h

mkdir -p hs/hs-vector/generated/Vector

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-vector/generated/Vector/Rotate.hs \
    --external-binding-spec external/vector.yaml \
    --module Vector.Rotate \
    vector_rotate.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-vector/generated/Vector/Length.hs \
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
    -o hs/hs-game/generated/Game/State.hs \
    --gen-binding-spec external/game.yaml \
    --module Game.State \
    game_internal.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-game/generated/Game/World.hs \
    --external-binding-spec external/game.yaml \
    --module Game.World \
    game_world.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-game/generated/Game/Player.hs \
    --external-binding-spec external/game.yaml \
    --module Game.Player \
    game_player.h
