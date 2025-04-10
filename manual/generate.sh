#!/bin/bash

PROJECT_ROOT=..

cabal run hs-bindgen-cli -- \
  preprocess \
    -i manual_examples.h \
    -I ${PROJECT_ROOT}/hs-bindgen/examples \
    -o hs/generated/Example.hs \
    --module Example
