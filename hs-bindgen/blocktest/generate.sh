#!/bin/bash

cabal run -- \
  hs-bindgen-cli preprocess \
   -I c \
   -o hs/generated/Iterator.hs \
   --module Iterator \
   --standard c23 \
   --enable-blocks \
   iterator.h
