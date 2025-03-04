# `clang-ast-dump`

This utility displays low-level details about the Clang AST.  It aids in the
design of our Haskell types and the implementation of the translation from
the Clang types to our Haskell types.

## Usage

All of the following commands are to be run in the project root.

Display options:

```
$ cabal run clang-ast-dump -- --help
```

Run:

```
$ cabal run clang-ast-dump -- hs-bindgen/examples/comments.h
```

## Alternatives

Official:

```
$ clang -Xclang -ast-dump foo.c
```
