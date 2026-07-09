# libgit2 example

A high-level Haskell binding to [libgit2](https://libgit2.org/), built on top of
hs-bindgen's generated low-level bindings using the `ToHighLevel` combinator
library in `hs-bindgen-runtime`.

This example is a stress-test: it takes a real, large, stateful C library (~60
public headers, a global init/shutdown lifecycle, opaque handles, an error model
in thread-local storage, a stateful iterator) and builds an idiomatic API from the
ground up. The point is the write-up in **[FINDINGS.md](FINDINGS.md)**: where the
combinators help, where they need hand-written glue, and what hs-bindgen could
generate instead. Start there if you want the conclusions.

## What is in here

```
libgit2/                    libgit2 source (git submodule, pinned to v1.9.0)
build-libgit2.sh            builds a dependency-light libgit2.so via cmake
generate-and-run.sh         generate bindings (topological order), build, run demos
shell.nix                   cmake + C toolchain, for building libgit2 by hand
FINDINGS.md                 the report: what we learned (read this)
hs-project/
  src/Generated/            low-level bindings, one module per header (generated)
  src/LibGit2/              the hand-written high-level API
  src/LibGit2.hs            umbrella module re-exporting the high-level API
  app/LogHigh.hs  LogLow.hs       `git log`, high-level vs low-level
  app/CommitHigh.hs CommitLow.hs  init + first commit, high-level vs low-level
```

The high-level API is small and worth reading as a tour of the combinators:

* `LibGit2.Types`   handle newtypes (`ForeignPtr`-backed), `Oid`, `Signature`, `GitTime`
* `LibGit2.Error`   the `GitError` exception and the status-checking closer
* `LibGit2.Marshal` the marshaller toolkit (`outHandle`, `handleIn`/`handleInC`, ...)
* `LibGit2.Git`     the `Git` monad (a `ReaderT` over the repository handle)
* `LibGit2.Signature` reading and writing the signature struct
* `LibGit2.Repository`, `.Object`, `.Revwalk`, `.Commit`, `.Reference`, `.Write`

## Requirements

* The hs-bindgen dev shell (`nix develop` at the repository root), which provides
  GHC, cabal, and libclang.
* `cmake` and a C toolchain for building libgit2. The scripts pull these from
  nixpkgs automatically (`nix shell nixpkgs#cmake nixpkgs#gnumake nixpkgs#gcc`), so
  nothing needs to be installed system-wide.
* `tsort` and `awk` (coreutils / gawk), used to derive the header generation order.

## Quick start

From the repository root, inside `nix develop`:

```bash
git submodule update --init examples/libgit2/libgit2
cd examples/libgit2
./generate-and-run.sh
```

That script builds libgit2 from source, generates the low-level bindings in
dependency order, builds the high-level library and the four demo programs, and
runs the demos. It is idempotent.

To run a demo by hand afterwards (the shared library must be on the load path):

```bash
cd hs-project
export LD_LIBRARY_PATH="$PWD/../libgit2/build:$LD_LIBRARY_PATH"
cabal run libgit2-log-high  -- /path/to/some/repo
cabal run libgit2-commit-high -- /tmp/libgit2-demo
```

## The two demos

* **`git log`** walks history from HEAD and prints `oid  author  summary` per
  commit. `LogHigh.hs` uses the `Git` monad and the high-level wrappers;
  `LogLow.hs` does the same with raw generated bindings and manual memory
  management. Run against the same repository they print identical output.

* **init + commit** creates a repository, writes a blob, builds a tree, and makes
  the first commit, then reads it back. With fixed inputs `CommitHigh.hs` and
  `CommitLow.hs` produce the same commit oid, which real `git` accepts. This is the
  cross-check that the struct-write combinators are byte-correct.

## How generation works

`generate-and-run.sh` does not hard-code the header order. It asks the tool for the
include graph and topologically sorts it:

```bash
hs-bindgen-cli info include-graph -I libgit2/include --show-paths -o include-graph.mmd git2.h
# parse the Mermaid edges, then `tsort`, then generate each header in that order,
# feeding earlier headers' binding specs as --external-binding-spec
```

63 of libgit2's 66 generatable headers come through cleanly. See FINDINGS.md for
the details, including the one real include cycle (`oid.h` and `types.h`).

## Building libgit2 only

```bash
./build-libgit2.sh          # produces libgit2/build/libgit2.so
```

It configures libgit2 with HTTPS and SSH off and the builtin regex/zlib/http-parser
backends, so the build needs no openssl, libssh2, pcre, or zlib.
