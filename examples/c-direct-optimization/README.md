# Direct Optimization Haskell Bindings

This example demonstrates generating Haskell bindings for the
direct-optimization C library from [POY 4.0][poy], which performs pairwise
string alignment of sequences over a small alphabet — the inner loop of
phylogenetic tree search.

[poy]: https://en.wikipedia.org/wiki/POY_(software)

Unlike most other examples, the C library is **vendored** under
`hs-project/direct-optimization` and compiled into the package with
`c-sources`, so there is nothing to fetch or install. See
[`bundled-c`](../bundled-c) for the same approach.

## Licensing

The Haskell code in this example is BSD-3-Clause, as elsewhere in `hs-bindgen`
(`hs-project/LICENSE`). **The vendored C library is not.**

`hs-project/direct-optimization` is part of POY 4.0, copyright 2007 Andrés
Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler, and the American Museum of
Natural History, and is licensed GPL-2.0-or-later. Its licence text is in
`hs-project/direct-optimization/COPYRIGHT`.

This is the only example in the repository that ships third-party copyleft
source, and that code is compiled into the example executable.

## Prerequisites

None beyond the standard `hs-bindgen` setup (GHC, Cabal, LLVM/Clang). No
external C libraries need to be installed.

## Running the Example

```bash
./generate-and-run.sh
```

This script will:

1. Generate Haskell bindings using `hs-bindgen`
2. Build the Haskell package, compiling the vendored C sources alongside it
3. Run the example program on a sample pair of DNA sequences

The program aligns two IUPAC DNA sequences given as arguments:

```console
$ cabal run c-direct-optimization-bin -- ACGTRYKM ACTTRYK
  seq-1   ACGTRYKM
  median  ACKTRYKm
  seq-2   ACTTRYK█

Alignment cost: 2
```

Each input character is an ambiguity group: `A C G T` are unambiguous, `R Y S W
K M` are two-base ambiguities, `B D H V` three-base, `N` any base, `-` a gap,
and `?` any base or gap. The lowercase form of any of these adds the gap state
to the group. The table itself lives in `Example.Alphabet.IUPAC`,
which also provides RNA and amino-acid variants.

In the output, `█` marks a position where that sequence has no state because
the alignment inserted or deleted there, as distinct from `-`, which is a gap
state present in the data.
