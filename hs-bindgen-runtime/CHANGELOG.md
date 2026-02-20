# Revision history for hs-bindgen-runtime

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Remove `withPtr` for `IncompleteArray` and `ConstArray`. Use `withElemPtr`
  from the `IsArray` class instead. See [PR #1712][pr-1712].

### New features

* Add new `safeCastFunPtr` function to `HsBindgen.Runtime.Prelude`

* Add new `IsArray` class with instances for `IncompleteArray` and
  `ConstArray`. See [PR #1712][pr-1712].

### Minor changes

* Add an internal prelude, re-exporting all definitions required by `hs-bindgen`
  generated code.

* Remove `TypeEquality` module and `TyEq`. Use built-in `(~)` or operator
  `(~)` (for later versions of GHC; implicitly imported from `Prelude`).

### Bug fixes

[pr-1712]: https://github.com/well-typed/hs-bindgen/pull/1712

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
