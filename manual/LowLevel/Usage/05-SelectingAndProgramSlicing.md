# Selecting and program slicing

The determination of which C declarations to translate is a complex problem. In
`hs-bindgen`, we split the process into three steps:

1. We specify a list of input C headers, which we refer to as _main headers_.
   `hs-bindgen` instructs Clang to read these main headers including all
   declarations therein, and transitive dependencies. `hs-bindgen` parses and
   reifies these declarations.

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

## Select predicates

After parsing and reifying the declarations, the frontend of `hs-bindgen` sorts
the declarations, handles macros, [provides names to
declarations](../Translation/01-GeneratedNames.md) and resolves and applies
[binding specifications](06-BindingSpecifications.md). Then, it matches a
_select predicate_, further reducing the number of declarations to be
translated.

_Select predicates_ allow fine-grained control about which declarations to
select for translation. For example, select predicates allow matching against
the qualified C names of declarations. In particular, the command-line arguments
to specify select predicates are (excerpt of `hs-bindgen-cli preprocess
--help`):

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
> of the outer `struct`. In particular, select predicates do not support the `@`
> syntax for specifying anonymous declarations like binding specifications do.

> [!NOTE]
> Select predicates, just like binding specifications, match against the
> (qualified) names used in the C code. In particular, select predicates do not
> match against the Haskell names possibly assigned by prescriptive binding
> specifications.

> [!NOTE]
> Path separators (forward slash on POSIX platforms and backslash on Windows)
> can be tricky when used in regular expressions.

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
- Select predicate: Match all declarations in the main header(s); corresponding
  to `main.h`.
- Program slicing: Do not select transitive dependencies.

The header files in `/path/to/simple/**` may use declarations from other
transitively included headers not residing in `/path/to/simple/**` (e.g.,
standard headers).
