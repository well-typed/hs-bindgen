# Revision history for hs-bindgen-runtime

## ?.?.? -- YYYY-mm-dd

### Breaking changes

* Rename `HsBindgen.Runtime.Internal.*` modules to `HsBindgen.Runtime.Support.*`;
  `Internal.Prelude` is now `HsBindgen.Runtime.Support`.
* `setUnionPayload` would previously generate a zeroed out byte array and write
  a `Storable` value to it. It now takes a byte array argument that the
  `Storable` value is written to. See [issue #2183][is-2183].

### New features

* Add a new `IsUnion` class for C unions. Currently the only member function is
  `zero`. See [issue #2060][is-2060] and [PR #2091][pr-2091].
* Add `get`/`set` functions that serve as getters/setters for union values.
  These depend `HasField` instances. See [issue #2060][is-2060] and [PR
  #2091][pr-2091].
* Add a new `HsBindgen.Runtime.Overloading` module that restores the default
  environment when using the `RebindableSyntax` language extension for
  overloaded record updates. See [issue #2085][is-2085] and [PR #2168][pr-2168].
* Add a new `IsStruct` class for C structs. Currently the only member function
  is `zero`. See [issue #2121][is-2121] and [PR #2164][pr-2164].
* Add `IsStructViaReadRaw` and `IsUnionViaReadRaw` helper types for deriving
  `IsStruct` and `IsUnion` respectively via a `ReadRaw` (and `StaticSize`)
  instance. [issue #2121][is-2121] and [PR #2164][pr-2164].
* Add new `setUnionPayloadBits` and `getUnionPayloadBits` functions for setting
  and getting bit-fields in unions. See [issue #1253][is-1253].
* Add `withFunPtrAs` to `HsBindgen.Runtime.Support.FunPtr`. `withFunPtrAs` is
  useful for callbacks whose own type has no `ToFunPtr` instance. Calls
  `withFunPtr` provided the callback is `Coercible`.

### Minor changes

* Deprecate `zeroUnionValue` in favour of the `zero` function from the new
  `IsUnion` class. See [issue #2060][is-2060] and [PR #2091][pr-2091].

### Bug fixes

* Fix a missing conversion from bytes to bits in the internals of
  `pokeBitOffWidth`. This would sometime cause the function to write more bytes
  than necessary to a pointer. See [PR #2169][pr-2169].

[is-1253]: https://github.com/well-typed/hs-bindgen/issues/1253
[is-2060]: https://github.com/well-typed/hs-bindgen/issues/2060
[is-2085]: https://github.com/well-typed/hs-bindgen/issues/2085
[is-2121]: https://github.com/well-typed/hs-bindgen/issues/2121
[is-2183]: https://github.com/well-typed/hs-bindgen/issues/2183
[pr-2091]: https://github.com/well-typed/hs-bindgen/pull/2091
[pr-2164]: https://github.com/well-typed/hs-bindgen/pull/2164
[pr-2168]: https://github.com/well-typed/hs-bindgen/pull/2168
[pr-2169]: https://github.com/well-typed/hs-bindgen/pull/2169

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
