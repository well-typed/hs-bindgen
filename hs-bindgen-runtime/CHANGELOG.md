# Revision history for hs-bindgen-runtime

## Unreleased

### New features

* Add a combinator library for hand-writing high-level wrappers over the
  generated low-level FFI bindings, in two modules under
  `HsBindgen.Runtime.HighLevel`. `Marshaller` is the marshaller vocabulary: a
  single `Marshal` arrow marshals a Haskell value into the C argument(s) it fills
  (build one with `scalar` or `bracket`, aim it with `at`, compose fields with
  `>>>` from the `Category` instance, and embed a sub-struct with `marshalNested`). A
  `MarshalStruct` seals a field chain into a whole-struct marshaller; `Unmarshaller`
  handles out-parameters; and `UnmarshalStruct` is the dual that reads C back into
  Haskell.
  Optional values cross the boundary with `marshalOptional` (write) and `unmarshalOptional`
  (read). `HighLevel` itself lifts a low-level function into a wrapper (`input` /
  `output` / `scratch` / result closers) and drops a struct marshaller into a
  wrapper position with `asArgument` / `asOutput` / `asResult`; a by-value struct
  argument is written into a zeroed slot, so its padding reaches C as zeros. Kept
  outputs accumulate into a flat result tuple (result last, up to eight
  components). Ready-made
  marshallers live in `Marshaller.Utils` and per-type defaults in `Defaults`, where
  `auto` is a single combinator that fills the mundane input positions and the
  closer of a wrapper from its high-level signature. It covers multi-C-argument
  defaults (such as a `ByteString` filling a `(const char *, size_t)` pair) and any
  user-defined `DefaultIn`, so only the interesting positions are written by hand;
  outputs stay explicit via `output`. The built-in defaults convert the idiomatic
  scalars (`Int`, `Word`, `Bool`, `Double`, `Float`) and the `String` / `ByteString`
  / `IncompleteArray` compounds, and pass through unchanged every raw C scalar type,
  every fixed-width type, and a raw pointer or function pointer (`Ptr` / `FunPtr`).
  In every position, including the result, the Haskell type in the signature picks the
  representation, so `auto` closes a wrapper that keeps the C return type (`IO CInt`)
  as readily as one that converts it (`IO Int`).
  The combinators (including `auto`) carry
  `INLINE` pragmas and fuse to the raw `alloca` / `poke` / `peek` at `-O2`.

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
