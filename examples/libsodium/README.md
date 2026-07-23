# Generate `libsodium` Haskell bindings with `hs-bindgen`

This example demonstrates generating Haskell bindings for
[libsodium](https://libsodium.org/), a modern, easy-to-use cryptography library.
It also hand-writes a small high-level API over the generated bindings.

## Prerequisites

Run from inside the hs-bindgen dev shell (`nix develop` at the repository root).
The build toolchain (autotools and a C compiler) is pulled from nixpkgs by the
script, so nothing else needs to be installed.

## Run the example

```bash
cd examples/libsodium
./generate-and-run.sh
```

This script will:

1. Fetch `libsodium` (pinned to `1.0.22-RELEASE`) and build it from source
2. Generate Haskell bindings using `hs-bindgen` (in topological header order)
3. Create `cabal.project.local`
4. Build the high-level library and the demo programs
5. Run the demos (low-level and high-level, which print identical output)
