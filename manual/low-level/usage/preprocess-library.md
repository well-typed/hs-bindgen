# Library-level invocation

## Overview
[t:overview]: #overview

The `preprocess-library` subcommand generates Haskell modules for a C library
whose public API spans multiple headers.  Where `preprocess` targets a single
header, `preprocess-library` walks the include graph of the given root
header(s), assigns each discovered header its own Haskell module, and runs
the binding generator once per module in dependency order.  Each step receives
the binding specifications from all previous steps as external binding
specifications, so cross-module type references resolve correctly.

This automates the multi-module workflow described in the [binding
specifications][manual:binding-specifications-multi] section.

## Basic usage
[t:basic-usage]: #basic-usage

```
hs-bindgen-cli preprocess-library \
    -I /usr/include \
    --library-root /usr/include/rpm \
    --hs-output-dir gen \
    --create-output-dirs \
    --overwrite-files \
    --module RPM \
    rpm/rpmlib.h
```

This command:

1. Parses `rpm/rpmlib.h` (resolved via `-I /usr/include`), which transitively
   includes all RPM public headers.
2. Walks the include graph to discover every header reachable from the root.
3. Filters headers to those under `--library-root /usr/include/rpm`.
4. Topologically sorts the filtered headers (leaves first).
5. For each header, derives a Haskell module name, constructs a selection
   predicate targeting that header's declarations, enables program slicing, and
   runs the binding generator.
6. Chains binding specifications: each step receives the binding specifications
   from all previous steps as external binding specifications, so cross-module
   type references resolve.

## Module-generation scope
[t:module-generation-scope]: #module-generation-scope

`--library-root DIR` (required, repeatable) defines which headers get their own
Haskell module.  A header in the include graph gets a module if and only if its
canonical path falls under a library root.

`--except-library-root PCRE` (optional, repeatable) excludes headers whose
canonical path matches the pattern, even when they are under a library root.
Types from excluded headers remain available to other modules through program
slicing and binding spec chaining.

```
hs-bindgen-cli preprocess-library \
    --library-root /usr/include/rpm \
    --except-library-root 'internal' \
    ...
```

This skips any header whose path contains "internal" (e.g.
`rpm/internal.h`).

Note: `-I` is the clang search path only; it tells clang where to find headers
during parsing and has no effect on which headers get modules.

## Selection predicates (declaration filtering)
[t:selection-predicates]: #selection-predicates

Selection predicates (`--select-by-header-path`, `--select-by-decl-name`,
`--select-except-deprecated`, etc.) control which *declarations* get bindings
within each generated module.  They are independent of `--library-root` and
`--except-library-root`, which control which *headers* get modules.

For example, `--select-except-deprecated` excludes deprecated declarations from
every module, but does not affect which headers get modules.

Because program slicing can pull types from any header (even outside the library
root) into a module, selection predicates cannot reliably determine which headers
produce output files.  Use `--library-root` and `--except-library-root` for
that.

## Module naming
[t:module-naming]: #module-naming

Module names are derived from each header's canonical path relative to the
`--library-root` directories (which are canonicalized before matching, so
symlinks and `..` segments are resolved).  The file extension is dropped, each
path component is capitalized, and the result is joined with dots under the
`--module` prefix.

| `--library-root` | Header path | Module name |
|---|---|---|
| `/usr/include/rpm` | `/usr/include/rpm/rpmlib.h` | `RPM.Rpmlib` |
| `/usr/include/rpm` | `/usr/include/rpm/rpmtypes.h` | `RPM.Rpmtypes` |
| `/usr/include` | `/usr/include/rpm/argv.h` | `RPM.Rpm.Argv` |

## Dry run and module listing
[t:dry-run]: #dry-run

`--dry-run` prints the processing plan (which headers produce which modules)
and exits without generating any files.  Useful for verifying the
`--library-root` and `--except-library-root` filters before running the full
generation.

`--list-modules` prints module names one per line (suitable for pasting into
a `.cabal` file) and exits.

## Module name collisions
[t:module-name-collisions]: #module-name-collisions

The naming scheme can produce collisions.  The subcommand detects them before
generating any files and exits with an error.  Known cases:

**First-character case folding.**  The naming step only uppercases the first
character of each path component, so two headers that differ only in that
character collide: `foo.h` and `Foo.h` both produce component `Foo`.  Note
that `foo.h` and `FOO.h` do *not* collide (`Foo` vs `FOO`), because the
remaining characters are left unchanged.

**Dot-slash equivalence.**  Only the last file extension is stripped, so a
header like `Widget.Core.h` retains a dot in the stem (`Widget.Core`).  That
dot becomes a module separator in the derived name, producing the same module
as `widget/core.h` would from two separate path components.  Any dot that
survives extension stripping is indistinguishable from a directory separator.

**Category overlap** (file-per-module mode only).  Each base module `M`
expands to category submodules `M.Safe`, `M.Unsafe`, `M.FunPtr`, and
`M.Global`.  If another header's derived base name matches one of those, the
Types file for that header and the category file for the first header write to
the same path.  For example, `foo.h` (module `M.Foo`) and `foo/safe.h`
(module `M.Foo.Safe`) collide at `M/Foo/Safe.hs`.

To resolve a collision, use `--except-library-root` to exclude one side, or
adjust the `--library-root` directories to change the derived relative paths.

<!-- sources and references -->

[manual:binding-specifications-multi]: binding-specifications.md#generating-multiple-modules
