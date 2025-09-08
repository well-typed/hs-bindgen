#!/usr/bin/env bash

select_flags=(
    # Main header.
    --select-by-header-path wlr/backend.h
    # Select transitive dependencies.
    --enable-program-slicing
)
cabal run -- hs-bindgen-cli preprocess wlr/backend.h \
    -I /nix/store/b9c70c82y2a4wq2r1l6896ral28al4iz-wlroots-0.19.0/include/wlroots-0.19/ \
    -v 3 \
    --clang-option -DWLR_USE_UNSTABLE \
    --parse-all \
    "${select_flags[@]}" \
    --module Generated.Wlroots \
    -o src/Generated/Wlroots.hs
