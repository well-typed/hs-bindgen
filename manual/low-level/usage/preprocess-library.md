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
    --hs-output-dir gen \
    --create-output-dirs \
    --overwrite-files \
    --module RPM \
    --library-root /usr/include/rpm \
    rpm/rpmlib.h
```

This command:

1. Parses `rpm/rpmlib.h` (resolved via `-I /usr/include`), which transitively
   includes all RPM public headers.
2. Walks the include graph to discover every header reachable from the root.
3. Filters to headers whose canonical path falls under `--library-root`.
4. Topologically sorts the remaining headers (leaves first).
5. For each header, derives a Haskell module name, constructs a selection
   predicate targeting that header's declarations, enables program slicing, and
   runs the binding generator.
6. Chains binding specifications: each step receives the binding specifications
   from all previous steps as external binding specifications, so cross-module
   type references resolve.

## Module naming
[t:module-naming]: #module-naming

Module names are derived from each header's path relative to the library root.
The file extension is dropped, each path component is capitalized, and the
result is joined with dots under the `--module` prefix.

| Library root | Header path | Module name |
|---|---|---|
| `/usr/include` | `/usr/include/widget/core.h` | `Widget.Widget.Core` |
| `/usr/include/widget` | `/usr/include/widget/core.h` | `Widget.Core` |
| `/usr/include/widget` | `/usr/include/widget/util/hash.h` | `Widget.Util.Hash` |

For flat directory layouts where all headers are directly under the library
root, the module name has a single component after the prefix:

| Library root | Header path | Module name |
|---|---|---|
| `/usr/include/rpm` | `/usr/include/rpm/rpmtypes.h` | `RPM.Rpmtypes` |
| `/usr/include/rpm` | `/usr/include/rpm/argv.h` | `RPM.Argv` |

## Library roots
[t:library-roots]: #library-roots

The `--library-root` option restricts module generation to headers under the
given directory.  Headers outside every library root are skipped (they may still
contribute types via program slicing).  The option is repeatable.

When no `--library-root` is given, the `-I` include directories are used as a
fallback.

Library roots are canonicalized internally (symlinks resolved, `..` segments
collapsed) to match the canonical paths that clang reports for headers in the
include graph.  The `-I` flags themselves are not canonicalized; they are passed
to clang as-is.

### Wide root warning

If a `--library-root` is not contained in any `-I` directory, a warning is
emitted.  A library root wider than the include search means that headers
outside the `-I` directories cannot be found by clang.

## Excluding headers
[t:excluding-headers]: #excluding-headers

The `--exclude-header` option takes a PCRE pattern.  Headers whose canonical
path matches the pattern are skipped entirely: no module is generated for them.
Types they define are still available to other modules through program slicing.

```
hs-bindgen-cli preprocess-library \
    --exclude-header 'internal' \
    ...
```

This skips any header whose path contains "internal" (e.g.
`widget/internal.h`).

## Dry run and module listing
[t:dry-run]: #dry-run

The `--dry-run` flag prints the processing plan without generating any files:

```
Plan: 3 headers -> 3 modules

  1. /usr/include/widget/types.h -> Widget.Types
  2. /usr/include/widget/core.h -> Widget.Core
  3. /usr/include/widget/util.h -> Widget.Util
```

The `--list-modules` flag prints generated module names one per line, useful
for pasting into a `.cabal` file:

```
Widget.Types
Widget.Core
Widget.Util
```

<!-- sources and references -->

[manual:binding-specifications-multi]: binding-specifications.md#generating-multiple-modules
