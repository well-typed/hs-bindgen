#!/usr/bin/env bash

PROJECT_ROOT=..

echo "Basic examples"

cabal run hs-bindgen-cli -- \
  preprocess \
    -I ${PROJECT_ROOT}/hs-bindgen/examples \
    -o hs/manual/generated/Example.hs \
    --module Example \
    manual_examples.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/manual/generated/Structs.hs \
    --module Structs \
    structs.h

echo "External bindings: vector example"

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-vector/generated/Vector.hs \
    --gen-external-bindings external/vector.yaml \
    --module Vector \
    vector.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-vector/generated/Vector/Rotate.hs \
    --external-bindings external/vector.yaml \
    --module Vector.Rotate \
    vector_rotate.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-vector/generated/Vector/Length.hs \
    --external-bindings external/vector.yaml \
    --module Vector.Length \
    vector_length.h

echo "External bindings: game example"

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-game/generated/Game/State.hs \
    --gen-external-bindings external/game.yaml \
    --module Game.State \
    game_internal.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-game/generated/Game/World.hs \
    --external-bindings external/game.yaml \
    --module Game.World \
    game_world.h

cabal run hs-bindgen-cli -- \
  preprocess \
    -I c \
    -o hs/hs-game/generated/Game/Player.hs \
    --external-bindings external/game.yaml \
    --module Game.Player \
    game_player.h
