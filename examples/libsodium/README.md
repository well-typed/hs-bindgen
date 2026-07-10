# Generate `libsodium` Haskell bindings with `hs-bindgen`

This example demonstrates generating Haskell bindings for
[libsodium](https://libsodium.org/), a modern, easy-to-use cryptography library.
It also hand-writes a small high-level API over the generated bindings; see
[FINDINGS.md](FINDINGS.md) for the write-up.

## Prerequisites

Run from inside the hs-bindgen dev shell (`nix develop` at the repository root).
The build toolchain (autotools and a C compiler) is pulled from nixpkgs by the
script, so nothing else needs to be installed.

## Run the example

```bash
git submodule update --init examples/libsodium/libsodium
cd examples/libsodium
./generate-and-run.sh
```

This script will:

1. Build `libsodium` from source
2. Generate Haskell bindings using `hs-bindgen` (in topological header order)
3. Create `cabal.project.local`
4. Build the high-level library and the demo programs
5. Run the demos (low-level and high-level, which print identical output)
