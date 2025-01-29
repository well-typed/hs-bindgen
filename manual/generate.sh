#!/bin/bash

cabal run hs-bindgen -- \
  preprocess \
    -i c/example.h \
    -o hs/generated/Example.hs \
    --module Example \
    --select-by-filename 'c/.*'
