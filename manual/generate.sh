#!/usr/bin/env bash

PROJECT_ROOT=..

# Basic examples

cabal run hs-bindgen-cli -- \
  preprocess \
    -i manual_examples.h \
    -I ${PROJECT_ROOT}/hs-bindgen/examples \
    -o hs/manual/generated/Example.hs \
    --module Example

cabal run hs-bindgen-cli -- \
  preprocess \
    -i structs.h \
    -I c \
    -o hs/manual/generated/Structs.hs \
    --module Structs

# External bindings: vector example

cabal run hs-bindgen-cli -- \
  preprocess \
    -i vector.h \
    -I c \
    -o hs/hs-vector/generated/Vector.hs \
    --gen-external-bindings external/vector.yaml \
    --module Vector

cabal run hs-bindgen-cli -- \
  preprocess \
    -i vector_rotate.h \
    -I c \
    -o hs/hs-vector/generated/Vector/Rotate.hs \
    --external-bindings external/vector.yaml \
    --module Vector.Rotate

cabal run hs-bindgen-cli -- \
  preprocess \
    -i vector_length.h \
    -I c \
    -o hs/hs-vector/generated/Vector/Length.hs \
    --external-bindings external/vector.yaml \
    --external-bindings external/length.yaml \
    --module Vector.Length

# External bindings: game example

cabal run hs-bindgen-cli -- \
  preprocess \
    -i game_internal.h \
    -I c \
    -o hs/hs-game/generated/Game/State.hs \
    --module Game.State

cabal run hs-bindgen-cli -- \
  preprocess \
    -i game_world.h \
    -I c \
    -o hs/hs-game/generated/Game/World.hs \
    --external-bindings external/game.yaml \
    --module Game.World

cabal run hs-bindgen-cli -- \
  preprocess \
    -i game_player.h \
    -I c \
    -o hs/hs-game/generated/Game/Player.hs \
    --external-bindings external/game.yaml \
    --module Game.Player
