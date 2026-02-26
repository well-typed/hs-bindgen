# Revision history for hs-bindgen

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Occurrences of the `CFieldType`/`CBitfieldType` type families in
  class instance heads are now replaced by their definition.
* `--enable-blocks` CLI option renamed to `-fblocks`, matching `clang`

### New features

* Generate explicit export lists in preprocessor-generated modules, hiding
  internal `hs_bindgen_` helper bindings from the public API and documentation
  ([#76](https://github.com/well-typed/hs-bindgen/issues/76)). Export items
  are module-qualified (e.g. `Example.myFunc`) to avoid ambiguity with
  Prelude names.
* Add `--post-qualified-imports` flag to generate post-qualified imports
  (`import Data.Proxy qualified`) instead of pre-qualified imports. This adds
  the `ImportQualifiedPost` language extension to generated modules.

### Minor changes

* Re-export all global definitions used by `hs-bindgen` generated code from
  `hs-bindgen-runtime`. This may affect required packages when using
  `hs-bindgen` generated code. In particular, the packages `ghc-prim` and
  `primitive` are not required by `hs-bindgen` generated code anymore.
* Improve and disambiguate delayed parse trace messages.

### Bug fixes

* Wrap function names in parentheses in generated C wrappers (`(erf)(x)`
  instead of `erf(x)`) to prevent function-like macro expansion when a macro
  shadows the function name.
* Fix incorrect enum constant values for enums with unsigned underlying types
  (e.g. `enum : uint8_t`). Values above the signed range (such as 128 or 255
  for `uint8_t`) were incorrectly stored as negative numbers because
  `hs-bindgen` used the signed libclang API.
* Include `FunPtr` for macro-defined newtypes.
* Fix a panic that occurred in some cases when generating `_Aux` newtypes for
  function pointers. See [issue #1694][issue-1694] and [PR #1724][pr-1724].

[issue-1694]: https://github.com/well-typed/hs-bindgen/issues/1694
[pr-1724]: https://github.com/well-typed/hs-bindgen/pull/1724

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
