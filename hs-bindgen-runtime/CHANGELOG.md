# Revision history for hs-bindgen-runtime

## 0.1.0 -- YYYY-mm-dd

* NON-BREAKING: add new `safeCastFunPtr` function to `HsBindgen.Runtime.Prelude`
* NON-BREAKING: add new `IsArray` class with instances for `IncompleteArray` and
  `ConstArray`.
* BREAKING: remove `withPtr` for `IncompleteArray` and `ConstArray`. Use
  `withElemPtr` from the `IsArray` class instead.