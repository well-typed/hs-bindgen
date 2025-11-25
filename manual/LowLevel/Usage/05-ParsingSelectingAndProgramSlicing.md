# Parsing, selecting and program slicing

The determination of which C declarations to translate is a complex problem. In
`hs-bindgen`, we split the process into four steps:

1. We specify a list of input C headers, which we refer to as _main headers_.
   `hs-bindgen` instructs Clang to read these main headers including all
   declarations therein, and transitive dependencies.

2. We do not parse and reify all declarations read and provided by Clang.
   Instead, we match declarations against [_parse
   predicates_](#parse-predicates). The parse predicate dictates which
   declarations `hs-bindgen` reifies into `hs-bindgen`-specific data structures.
   We parse and reify declarations matching the parse predicate, and do not
   attempt to parse and reify declarations not matching the parse predicate.

3. Given the set of declarations parsed and reified by `hs-bindgen`, [_select
   predicates_](#select-predicates) dictate which declarations to generate
   bindings for.

4. Now we have the set of selected declarations to generate bindings for. If
   [_program slicing_](#program-slicing) is disabled, `hs-bindgen` only
   generates bindings for these declarations. If, instead, program slicing is
   enabled, `hs-bindgen` also selects and generates bindings for transitive
   dependencies. Transitive dependencies may be located in non-main headers such
   as other library headers or system headers.

## Main headers

Users can specify _main headers_ via command line arguments, by passing them to
the library function `hsBindgen`, or via `hashInclude` expressions when using
the Template Haskell interface. Then, `hs-bindgen` instructs Clang to read
these main headers including all declarations therein, and transitive
dependencies.

> [!NOTE]
> The manual section [Includes](./04-Includes.md) describes how headers are
> resolved.

## Parse predicates

Parse predicates control if `hs-bindgen` parses and reifies a declaration read
by Clang into `hs-bindgen`-specific data structures. `hs-bindgen` knows the
definitions of parsed declarations. However, `hs-bindgen` does not know much
about declarations it does not attempt to parse, or fails to parse. `hs-bindgen`
registers those declarations as _parse-not-attempted_ and _parse-failed_,
respectively. These declarations can be made available to `hs-bindgen` using
[external bindings](./06-BindingSpecifications.md). Anonymous declarations must
be parsed to use an external binding for them, however. They must be named in
order to be specified in an external binding specification.

We use parse predicates mostly because:

- We seek to avoid repetitive parsing and reification. We can do so by using
  binding specifications. For example, we avoid parsing the standard library
  every time, and provide external bindings covering the standard library. Also,
  declarations may be in external libraries for which the user has external
  bindings.
- Not all declarations can be parsed and reified by `hs-bindgen`. We may want to
  exclude such declarations and instead provide manual external bindings for
  them.

Parse predicates are coarse and only allow matching against the header path
containing the declaration being matched. By default `hs-bindgen` only parses
and reifies declarations in headers from sub-directories of the main headers,
and avoids parsing and reifying all declarations. This default behavior can be
changed; in particular, the command line options are:

- `--parse-all`: Parse all declarations provided by Clang to `hs-bindgen`.
- `--parse-from-main-headers`: Only parse declarations in main headers.
- `--parse-from-main-header-dirs`: Parse declarations in main headers and
  transitively included headers from sub-directories of main headers (default).
- `--parse-by-header-path PCRE` and `--parse-except-by-header-path PCRE`: Parse
  or do not attempt to parse declarations in headers with paths matching the
  regular expression `PCRE`, respectively.

Also the `hs-bindgen` library and the Template Haskell interface allow direct
specification of the parse predicate using the data type `ParsePredicate`.

> [!NOTE]
> Parse predicates do not apply to all declarations. In particular, `hs-bindgen`
> always parses declaration required for scoping; these are, for example, type
> definitions. These declarations will be filtered out during selection.

> [!NOTE]
> Parse predicates match against header _paths_, and not just header filenames.
> The paths are determined by Clang, dependent on the C include path
> directories. They may be absolute or relative. The manual section
> [Includes](./04-Includes.md) describes how headers are resolved.

Parse predicates do not support matching on declaration names because names are
only determined later in the translation process; in particular, after name
mangling and resolving and applying binding specifications. Use [_select
predicates_](#select-predicates) to match on declaration names.

> [!NOTE]
> Path separators (forward slash on POSIX platforms and backslash on Windows)
> can be tricky when used in regular expressions.

## Select predicates

After parsing the declarations, the frontend of `hs-bindgen` sorts the parsed
declarations, handles macros, [provides names to
declarations](../Translation/01-GeneratedNames.md) and resolves and applies
[binding specifications](06-BindingSpecifications.md). Then, it matches a
_select predicate_, further reducing the number of declarations to be
translated.

_Select predicates_ allow fine-grained control about which declarations to
select for translation. For example, select predicates allow matching against
declaration names. In particular, the command-line arguments to specify select
predicates are (excerpt of `hs-bindgen-cli preprocess --help`):

```text
--select-all             Select all declarations
--select-from-main-headers
                         Select declarations in main headers (default)
--select-from-main-header-dirs
                         Select declarations in main header directories
--select-by-header-path PCRE
                         Select declarations in headers with paths that match
                         PCRE
--select-except-by-header-path PCRE
                         Select except declarations in headers with paths that
                         match PCRE
--select-by-decl-name PCRE
                         Select declarations with C names that match PCRE
--select-except-by-decl-name PCRE
                         Select except declarations with C names that match
                         PCRE
```

> [!NOTE]
> For anonymous declarations, the select predicate matches against the
> _use-sites_ of the anonymous declarations. For example, to select an anonymous
> inner `struct` together with the named outer `struct`, match against the name
> of the outer `struct`.

> [!NOTE]
> We match select predicates before handling C `typedef`s. That is, when
> matching select predicates, we have not yet renamed types, and select
> predicates must use names before renaming. This is consistent with binding
> specifications.

## Program slicing

Let us translate a specific declaration --- for example, the function
declaration

```c
enum FileOperationStatus read_file_chunk(FileType *file_ptr, void *buffer,
                                         size_type bytes_to_read);
```

The translation of the `read_file_chunk` function declaration is useful only if
the _transitive dependencies_ `FileOperationStatus`, `FileType`, and `size_type`
are available as well. For a given set of selected declarations, _program
slicing_ determines and selects the corresponding transitive dependencies.

> [!NOTE]
> Parse predicates are _always_ used, even when program slicing is enabled.
> Transitive dependencies must therefore either match the parse predicate so
> they can be selected for translation, or be made available to `hs-bindgen` by
> an external binding.

For example, assume the enumeration type `FileOperationStatus` is defined in an
external library. We can generate bindings for `FileOperationStatus` by changing
the default parse predicate so that declarations of the external library are
also parsed and reified by `hs-bindgen`. Instead, we could also provide an
external binding for `FileOperationStatus`.

> [!NOTE]
> Program slicing can cause declarations to be included even if they are
> explicitly deselected by a select predicate.

## Notes and examples

### Example use case and default behavior of `hs-bindgen`

Assume we generate bindings for a library `simple` with main header `simple/main.h`.
We use the following command line

```console
hs-bindgen-cli preprocess -I /path/to/simple main.h
```

By default, `hs-bindgen` the command-line client uses the following configuration:

- Main header: `main.h`.
- Parse predicate: Match all declarations in header(s) in subdirectories of the
  main header; corresponding to `/path/to/simple/**`.
- Select predicate: Match all declarations in the main header(s); corresponding
  to `main.h`.
- Program slicing: Do not select transitive dependencies.

> [!NOTE]
> The default parse predicate matches all headers in `/path/to/simple/**`.
> However, this does not mean we actually parse all headers matching this
> predicate. Instead, we only parse _the ones that happen to be transitively
> included_ by the main headers specified.

The header files in `/path/to/simple/**` may use declarations from other
transitively included headers not residing in `/path/to/simple/**` (e.g.,
standard headers). By default, `hs-bindgen` does not parse declarations in these
"external" headers, but users can provide external bindings for _non-parsed_
declarations.

### Parse vs select predicates

The parse predicate and the select predicate both allow matching against header
paths but serve different purposes. The parse predicate dictates which
declarations `hs-bindgen` reifies into `hs-bindgen`-specific data structures,
the select predicate dictates which declarations `hs-bindgen` generates bindings
for.

Example: Assume program slicing is enabled. All of the following scenarios
are different:

- Parse `library/*.h`, select `library/main.h`.

- Parse `library/main.h`, select all.

- Parse `library/*.h`, select all.
