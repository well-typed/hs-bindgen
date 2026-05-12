# Alpine Linux dev container

This directory provides a containerised Alpine Linux build environment for
`hs-bindgen`. It exists for two reasons:

1. **Tight local feedback loop** — iterate on Alpine/musl issues without
   waiting on CI.
2. **Single source of truth** — `Dockerfile` is consumed both by this local
   loop and by `.github/workflows/alpine.yml`, so local and CI environments
   are bit-for-bit identical.

See [#1817](https://github.com/well-typed/hs-bindgen/issues/1817) for context.

## Quick start

From the repository root:

```bash
# Interactive shell inside the container, with the repo mounted at /repo:
./dev/alpine/enter.sh

# One-shot command:
./dev/alpine/enter.sh cabal build all
./dev/alpine/enter.sh cabal test all --test-show-details=direct
```

The first invocation builds the image. Cold build is ~5 minutes (most of
it spent compiling doxygen 1.15.0 from source against musl — the upstream
binary tarball is glibc-linked, and Alpine's apk doxygen lags). Subsequent
runs reuse the layer cache.

## What's inside

- Alpine 3.21 base
- GHC 9.8.2 + cabal-install 3.10.3 (apk-packaged)
- LLVM/clang 19, libclang.so under `/usr/lib`
- Doxygen 1.15.0 (built from source under `/usr/local/bin` to match the
  version pinned by the rest of CI)
- musl-dev, gcc, gmp, ncurses, zlib (with static archives), libpcap-dev,
  libdispatch-dev (for the blocktest example)
- `LIBCLANG_PATH=/usr/lib`, `LLVM_CONFIG=/usr/lib/llvm19/bin/llvm-config`

The cabal store inside the container lives in a Docker volume
(`hs-bindgen-alpine-cabal-store`), separate from the host store. This avoids
mixing musl-built and glibc-built artifacts. Remove the volume to start
fresh: `docker volume rm hs-bindgen-alpine-cabal-store`.

## When to update

- **Bumping Alpine:** edit `FROM alpine:3.21` in `Dockerfile`, re-run
  `cabal build all` and `cabal test all` inside the container, fix any
  drift in apk package names.
- **Adding a build dep:** add it to the `apk add` line in `Dockerfile`.
  Both local dev and CI pick up the change automatically.

## Requirements on the host

- Docker or Podman on `PATH`.
- ~3 GB free disk space for the image and the in-container cabal store.

## Known caveats

- **Files written by the container are owned by root.** The container runs as
  root by default; artifacts under `dist-newstyle/` and `examples/*/generated/`
  end up root-owned on the host. `sudo rm -rf` (or another container run with
  `rm`) to clean up.

- **`.ghc.environment.*` files at the repo root are cleared on entry.**
  Cabal writes `.ghc.environment.<arch>-<os>-<version>` at the project root,
  and the fixture runner refuses to proceed when more than one coexists.
  Since host and container builds produce different files into the same
  bind-mounted tree, `enter.sh` removes all of them before each container
  start; the next `cabal build` inside the container regenerates the
  container's file. Re-run `cabal build all` on the host afterwards if you
  want the host file regenerated.
