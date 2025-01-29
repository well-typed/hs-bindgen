#!/bin/bash

cabal run hs-bindgen -- \
  preprocess \
    -i example.h \
    -I ./c \
    -o hs/generated/Example.hs \
    --module Example \
    --select-by-filename 'c/.*'
