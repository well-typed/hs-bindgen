# `clang-ast-dump`

This is a (likely temporary) utility that dumps the Clang AST with lots of
details.  I am using it to explore all of the data provided by Clang, which is
not all shown in other dump utilities.

Contact: Travis Cardwell <travis@well-typed.com>

## Usage

All of the following command are to be run in the project root.

Configure:

```
$ ln -s clang-ast-dump/cabal.project.local
```

Build:

```
$ cabal build clang-ast-dump
```

Display options:

```
$ cabal run clang-ast-dump -- --help
```

Run:

```
$ cabal run clang-ast-dump -- hs-bindgen/examples/comments.h
```

Deconfigure:

```
$ rm -f cabal.project.local
```

## Alternatives

Official:

```
$ clang -Xclang -ast-dump -fsyntax-only foo.c
```

`hs-bindgen ast-dump`:

```
$ cabal run hs-bindgen -- show-clang-ast -i hs-bindgen/examples/comments.h
```
