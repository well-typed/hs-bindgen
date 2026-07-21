# Generate `libgit2` Haskell bindings with `hs-bindgen`

This example demonstrates generating Haskell bindings for
[libgit2](https://libgit2.org/), a portable C implementation of Git. It also
hand-writes a high-level API over the generated bindings.

## Prerequisites

Run from inside the hs-bindgen dev shell (`nix develop` at the repository root).
The build toolchain (`cmake` and a C compiler) is pulled from nixpkgs by the
script, so nothing else needs to be installed.

## Run the example

```bash
cd examples/libgit2
./generate-and-run.sh
```

This script will:

1. Fetch `libgit2` (pinned to `v1.9.0`) and build it from source
2. Generate Haskell bindings using `hs-bindgen` (in topological header order)
3. Create `cabal.project.local`
4. Build the high-level library and the demo programs
5. Run the demos (`git log` and init + commit, low-level and high-level)
