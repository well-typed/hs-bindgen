# Revision history for hs-bindgen-runtime

## ?.?.? -- YYYY-mm-dd

### Breaking changes

### New features

* Add new `safeCastFunPtr` function to `HsBindgen.Runtime.Prelude`.

* Improve documentation and fix documentation-related warnings.

### Minor changes

* Add an internal prelude, re-exporting all definitions required by `hs-bindgen`
  generated code.

* Remove `TypeEquality` module and `TyEq`. Use built-in `(~)` or operator
  `(~)` (for later versions of GHC; implicitly imported from `Prelude`).

* Do not export `intVal` from `HsBindgen.Runtime.ConstantArray`.

### Bug fixes

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
