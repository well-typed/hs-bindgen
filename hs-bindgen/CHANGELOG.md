# Revision history for hs-bindgen

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Rename option `--enable-record-dot` to `--omit-field-prefixes`, which is more
  to the point.
* Occurrences of the `CFieldType`/`CBitfieldType` type families in
  class instance heads are now replaced by their definition.
* `--enable-blocks` CLI option renamed to `-fblocks`, matching `clang`
* Macro declarations are now handled internally using a separate namespace.
  Binding specifications and selection predicates now refer to macros using
  `macro`.  For example, a macro named `foo` is referred to as `macro foo`.
* For C function *declarations* that take arrays as arguments, generate Haskell
  function declarations that take pointers to their corresponding array
  *elements* as arguments. This uses the `Elem` associated type class from the
  new `IsArray` class. See [PR #1712][pr-1712].
* For C function *types* (.e.g, `typedef`s) that take arrays as arguments,
  generate Haskell function types (.e.g, `newtype`s) that take pointers to their
  corresponding array *elements* as arguments. This uses the `Elem` associated
  type class from the new `IsArray` class. See [PR #1712][pr-1712].
* Parse predicates have been removed. Now, `hs-bindgen` always parses and
  reifies all declarations, reducing code complexity and cognitive overhead
  imposed on users. To migrate, remove `--parse-*` command line client options,
  or the parse-related configuration option when using Template Haskell model.

### New features

* The `info include-graph` sub-command has received new options: `--include
  PCRE`, and `--exclude PCRE` allow fine-tuned choice of headers to include or
  exclude from the include graph; `--simple` reduces include graph verbosity,
  for example, by removing edge labels.
* Generate explicit export lists in preprocessor-generated modules, hiding
  internal `hs_bindgen_` helper bindings from the public API and documentation
  ([#76](https://github.com/well-typed/hs-bindgen/issues/76)). Export items
  are module-qualified (e.g. `Example.myFunc`) to avoid ambiguity with
  Prelude names.
* The command line client has a new `internal frontend` sub-command. It takes a
  `--pass` option (e.g. `internal frontend --pass select`), dumping the result
  of the provided frontend pass. Defaults to `adjust-types` (the final pass)
  when omitted.
* Add `--post-qualified-imports` flag to generate post-qualified imports
  (`import Data.Proxy qualified`) instead of pre-qualified imports. This adds
  the `ImportQualifiedPost` language extension to generated modules.
* Support top-level anonymous structs and enums as global variables
  (e.g., `struct { int x; int y; } point;`). The anonymous type is named
  after the global variable. Extern anonymous declarations
  (e.g., `extern struct { .. } config;`) are rejected as unusable.
  * Generate an `IsArray` instance for each newtype of a type with an `IsArray`
* Support unnamed bit-field declarations (of width zero), used to instruct the
  compiler to not pack any more fields into the current storage unit.
* Generate bindings for nested struct and union declarations even if we failed
  to generate bindings for the enclosing struct or union. See [PR
  #1849][pr-1849].
* Generate bindings for nested anonymous structs and unions. See [PR
  #1839][pr-1839] and [PR #1869][pr-1869].

### Minor changes

* Support `language-c` 0.9.x through 0.10.2
  ([#1662](https://github.com/well-typed/hs-bindgen/issues/1662)).
* Re-export all global definitions used by `hs-bindgen` generated code from
  `hs-bindgen-runtime`. This may affect required packages when using
  `hs-bindgen` generated code. In particular, the packages `ghc-prim` and
  `primitive` are not required by `hs-bindgen` generated code anymore.
* Improve and disambiguate delayed parse trace messages.
* Improve error handling in `hs-bindgen` frontend, see [issue #1009][is-1009]

### Bug fixes

* Fix generation of documentation for record fields in Template Haskell mode
  with `OmitFieldPrefixes`. Previously, duplicate record fields induced "ambiguous
  occurrence" errors. This fix is only available for GHC versions 9.8 and newer.
  For older versions of GHC, we deactivated creation of documentation for record
  fields in Template Haskell mode.
* Generate bindings for `static` (non-`const`) declarations. Previously these
  were rejected as an unsupported, even though the duplicate-symbols warning
  suggested using `static`
  ([#1769](https://github.com/well-typed/hs-bindgen/issues/1769)).
* Wrap function names in parentheses in generated C wrappers (`(erf)(x)`
  instead of `erf(x)`) to prevent function-like macro expansion when a macro
  shadows the function name.
* Fix incorrect enum constant values for enums with unsigned underlying types
  (e.g. `enum : uint8_t`). Values above the signed range (such as 128 or 255
  for `uint8_t`) were incorrectly stored as negative numbers because
  `hs-bindgen` used the signed libclang API.
* Include `FunPtr` for macro-defined newtypes. See [PR #1711][pr-1711].
* Fix a panic that occurred in some cases when generating `_Aux` newtypes for
  function pointers. See [issue #1694][is-1694] and [PR #1724][pr-1724].
* Fix a panic that occurred when the argument to a `#include` is a macro. In
  Haddock documentation of declarations in the included header, we just
  document the filename of the header.
* Fix `--create-output-dirs` not creating directories for `--gen-binding-spec`
  output paths. See [issue #1806][issue-1806].
* Skip functions whose parameters reference struct/union declarations that
  will not be visible outside of the function, including both forward
  references and inline definitions. This could indicate a missing `#include`
  in the C header. Previously, forward references caused a panic in
  `MangleNames`; now they are skipped with a warning.

[is-1009]: https://github.com/well-typed/hs-bindgen/issues/1009
[is-1694]: https://github.com/well-typed/hs-bindgen/issues/1694
[pr-1711]: https://github.com/well-typed/hs-bindgen/pull/1711
[pr-1712]: https://github.com/well-typed/hs-bindgen/pull/1712
[pr-1724]: https://github.com/well-typed/hs-bindgen/pull/1724
[issue-1806]: https://github.com/well-typed/hs-bindgen/issues/1806
[pr-1839]: https://github.com/well-typed/hs-bindgen/pull/1839
[pr-1849]: https://github.com/well-typed/hs-bindgen/pull/1849
[pr-1869]: https://github.com/well-typed/hs-bindgen/pull/1869

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
