# Parsing, selecting and program slicing

The determination of which C declarations to translate, and which C declarations
to exclude is a complex problem. In brief, we need to define which headers to
_parse_, and for a given set of headers, which declarations to _select_.

Further, we may want to translate and use a specific declaration --- for
example, the function declaration

```c
enum FileOperationStatus read_file_chunk(FILE *file_ptr, void *buffer,
                                         size_t bytes_to_read);
```


However, the translation of the `read_file_chunk` function declaration is useful
only if the _transitive dependencies_ `FileOperationStatus`, `FILE`, and
`size_t` are translated and available as well. For a given set of selected
declarations, _program slicing_ determines and selects the corresponding
transitive dependencies.

<!-- TODO: Maybe refer to the parse/select name mangle binding specification
resolution passes (if they are mentioned in the manual before this section). -->

In detail, `hs-bindgen` offers three "mechanisms" for specifying which
declarations to translate:
1. [_Parse predicates_](#parse-predicates) determine the headers to parse or
   exclude. We parse all declarations in parsed headers, and exclude all
   declarations in excluded headers.
2. Given the set of parsed declarations, [_selection
   predicates_](#selection-predicates) determine which declarations to select or
   exclude. We match selection predicates after renaming named and anonymous
   declarations and resolving and applying prescriptive and external binding
   specifications.
3. If [_program slicing_](#program-slicing) is enabled, we also select
   transitive dependencies (if they are not excluded by the selection predicate
   in step 2); if program slicing is disabled, we only select declarations in
   the main header files.

## Parse predicates

In general, users specify the main headers to parse using command line
arguments, or `hashInclude`. However, sometimes more fine-grained _parse
predicates_ determining the headers to parse or exclude are necessary.

In particular, with parse predicates users can specify to
  - only parse main headers (i.e., the headers provided on the command line or
    via `hashInclude`); `--parse-from-main-headers` command line option;
  - parse main headers and transitively included headers from subdirectories of
    main headers; `--parse-from-main-header-dirs` command line option;
  - parse headers with paths matching a regular expression;
    `--parse-by-header-path PCRE` and `--parse-except-by-header-path PCRE`
    command line options.

<!-- TODO: Describe the two options -->

<!-- TODO: I think we should rename the last option to
`--parse-exclude-by-header-path`. -->

> [!NOTE] Note that parse predicates match against header _paths_, and not just
> header filenames. The paths are determined by `libclang`, dependent on the C
> include path directories. They may be absolute or relative.

Parse predicates do not support matching on declaration names because names are
only determined later in the translation process, in particular, after name
mangling and resolving and applying prescriptive and external binding
specifications. Use [_selection predicates_](#selection-predicates) to match on
declaration names.

- [ ] TODO: Amend help of command line flags.
- [ ] TODO: Add TH configuration options (ParsePredicate?).

> [!NOTE] Note that path separators (forward slash on POSIX platforms and
> backslash on Windows) can be tricky when used in regular expressions.

## Selection predicates

* The `Select` pass will support selection of anonymous types. (This is not
  implemented as I write this, but it is coming soon. We need to decide on a
  syntax that works well with both regular expressions and YAML. #844)

* Since the `Select` pass is before the `HandleTypedefs` pass, types are not yet
  renamed. Predicates must use names before renaming. This is consistent with
  binding specifications.

* We may want to provide some examples of regular expressions for common
  patterns.

## Program slicing

* Predicates are *always* used, even when slicing. Any transitive dependency
  that needs to be selected via slicing must therefore match the `Select`
  predicate.

  For example, the `program_slicing_simple` test selects transitive dependency
  `uint32_t` from the standard library. The `Parse` predicate is configured to
  parse all declarations so that the `uint32_t` declarations is available for
  slicing in the `Select` pass.

## Other notes

* We now only use terminology "select" for the `Select` pass, and we use "parse"
  for the `Parse` pass. We use terminology "exclude" for negatives in both
  passes.

  Note that binding specifications still use terminology "require" (internal,
  not exposed to users) and "omit."
