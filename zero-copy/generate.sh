#!/bin/bash

cabal run -- hs-bindgen-cli preprocess \
  -I cbits \
  -o app/Generated.hs \
  --module Generated \
  cbits.h
