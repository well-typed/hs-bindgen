# Internal Commands

Development and debugging commands. Not part of the public interface.

## `internal frontend`

Dumps the result of a single frontend pass to stdout (via `Show`).

### Usage

```bash
hs-bindgen-cli internal frontend [--pass PASS] [OPTIONS] HEADER...
```

Accepts the same options as `preprocess`. The `--pass` option selects which
frontend pass to dump (defaults to `adjust-types`, the final pass).

### Available passes

| # | `--pass` value | Description |
|---|----------------|-------------|
| 1 | `parse` | Traverse libclang AST into Haskell C AST |
| 2 | `simplify-ast` | Convert anonymous enums to pattern synonyms |
| 3 | `assign-anon-ids` | Assign names to anonymous declarations |
| 4 | `construct-translation-unit` | Order declarations by dependencies, build graphs |
| 5 | `handle-macros` | Typecheck macros, reparse with macro info |
| 6 | `resolve-binding-specs` | Apply external/prescriptive binding specs |
| 7 | `mangle-names` | Assign Haskell names, squash typedefs |
| 8 | `select` | Filter by predicates and program slicing |
| 9 | `adjust-types` | Adjust types (e.g. function args to function pointers) |

Passes 1--3 produce `[ParseResult p]`; passes 4--9 produce
`C.TranslationUnit p`. See `HsBindgen.Frontend` module header for ordering
constraints.

### Examples

```bash
# After final pass (default)
cabal run hs-bindgen-cli -- internal frontend myheader.h

# After parse only
cabal run hs-bindgen-cli -- internal frontend --pass parse myheader.h

# With verbose tracing
cabal run hs-bindgen-cli -- -v3 internal frontend --pass select myheader.h

# Restrict to main header
cabal run hs-bindgen-cli -- internal frontend --pass adjust-types \
  --parse-from-main-headers /usr/include/time.h

# With an external binding spec
cabal run hs-bindgen-cli -- internal frontend --pass resolve-binding-specs \
  --external-binding-spec spec.yaml myheader.h
```

## `clang-ast-dump`

Displays low-level details about the Clang AST, as libclang sees it (before
any `hs-bindgen` processing). Aids in designing the translation from Clang
types to our Haskell types.

Only built with the `dev` flag:

```bash
cabal build all -f dev
```

### Usage

```bash
cabal run clang-ast-dump -- [OPTIONS] FILE
```

### Examples

```bash
cabal run clang-ast-dump -- myheader.h
cabal run clang-ast-dump -- --all myheader.h
cabal run clang-ast-dump -- --same-file myheader.h
```

### Alternatives

Official Clang AST dump:

```bash
clang -Xclang -ast-dump myheader.c
```

Source: `hs-bindgen/clang-ast-dump/Main.hs`.
