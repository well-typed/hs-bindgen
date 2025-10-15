# Populate additional environment variables required by `hs-bindgen`.

# NOTE: Use this setup hook when building packages with `hs-bindgen`. The client
# requires a separate wrapper (doh !) which is defined in `hs-bindgen-cli.nix`.
# Please keep this setup hook and the wrapper synchronized!
populateHsBindgenEnv() {
    # Inform `hs-bindgen` about Nix-specific `CFLAGS` and `CCFLAGS`. In contrast
    # to `rust-bindgen-hook.sh` (see Nixpkgs), we do not set `CXXFLAGS`.
    BINDGEN_EXTRA_CLANG_ARGS="$(<@clang@/nix-support/cc-cflags) $(<@clang@/nix-support/libc-cflags) $NIX_CFLAGS_COMPILE"
    export BINDGEN_EXTRA_CLANG_ARGS

    # Inform `hs-bindgen` that it does not have to perform heuristic search for
    # the builtin include directory. (We set the builtin include directory using
    # `BINDGEN_EXTRA_CLANG_ARGS`).
    BINDGEN_BUILTIN_INCLUDE_DIR=disable
    export BINDGEN_BUILTIN_INCLUDE_DIR
}

postHook="${postHook:-}"$'\n'"populateHsBindgenEnv"$'\n'
