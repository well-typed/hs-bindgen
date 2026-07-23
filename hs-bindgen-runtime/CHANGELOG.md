# Revision history for hs-bindgen-runtime

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Rename `HsBindgen.Runtime.Internal.*` modules to `HsBindgen.Runtime.Support.*`;
  `Internal.Prelude` is now `HsBindgen.Runtime.Support`.
* The high-level combinator library moved out of this package into the new
  `hs-bindgen-highlevel` package: modules previously under
  `HsBindgen.Runtime.HighLevel.*` are now `HsBindgen.HighLevel.*`. Depend on
  `hs-bindgen-highlevel` and update imports.

### New features

* Add a new `IsUnion` class for C unions. Currently the only member function is
  `zero`, which creates a union value set to all zeroes.
* Add `get`/`set` functions that serve as getters/setters for union values.
  These depend `HasField` instances. See [issue #2060][is-2060] and [PR
  #2091][pr-2091].

### Minor changes

* Deprecate `zeroUnionValue` in favour of the `zero` function from the new
  `IsUnion` class. See [issue #2060][is-2060] and [PR #2091][pr-2091].

### Bug fixes

None

[is-2060]: https://github.com/well-typed/hs-bindgen/issues/2060
[pr-2091]: https://github.com/well-typed/hs-bindgen/pull/2091

## 0.1.0-alpha2 -- 2026-03-27

### Breaking changes

* Remove `withPtr` for `IncompleteArray` and `ConstArray`. Use `withElemPtr`
  from the `IsArray` class instead. See [PR #1712][pr-1712].
* The `BitfieldPtr` constructor pattern is removed, making the type opaque.  A
  smart constructor and accessor functions are exported instead.
* `StaticSize` constraints are added to `HasCBitfield` API functions, in order
  to calculate memory bounds.  This enables single `peek`/`poke` reads/writes
  when possible while ensuring that neighboring memory is not accessed.

### New features

* Add new `safeCastFunPtr` function to `HsBindgen.Runtime.Prelude`.

* Improve documentation and fix documentation-related warnings.

* Add new `IsArray` class with instances for `IncompleteArray` and
  `ConstArray`. See [PR #1712][pr-1712].

### Minor changes

* Add an internal prelude, re-exporting all definitions required by `hs-bindgen`
  generated code.

* Remove `TypeEquality` module and `TyEq`. Use built-in `(~)` or operator
  `(~)` (for later versions of GHC; implicitly imported from `Prelude`).

* Do not export `intVal` from `HsBindgen.Runtime.ConstantArray`.

### Bug fixes

* Rewrite bit-field `peek` and `poke` code to read to and write from the correct
  locations in memory, support packed `struct` fields that cross machine word
  boundaries, and only use aligned reads/writes so that it is safe across all
  architectures
* Fix `loMask @Int64 64`, which was returning an incorrect mask

[pr-1712]: https://github.com/well-typed/hs-bindgen/pull/1712

## 0.1.0-alpha -- 2026-02-06

* First public pre-release.
