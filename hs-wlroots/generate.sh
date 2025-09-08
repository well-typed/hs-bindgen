#!/usr/bin/env bash

select_flags=(
    # Main header.
    --select-by-header-path wlr/backend.h
    # Select transitive dependencies.
    --enable-program-slicing
)
cabal run -- hs-bindgen-cli preprocess wlr/backend.h \
    -v 3 \
    --clang-option -DWLR_USE_UNSTABLE \
    --standard c23 \
    --parse-all \
    "${select_flags[@]}" \
    --module Generated.Wlroots \
    --hs-output-dir src
