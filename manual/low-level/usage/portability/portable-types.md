# Portable types

`hs-bindgen` generates datatype bindings that are platform-dependent. For
example, their implementation may reference platform-dependent [memory
layout][manual:portability-memory-layout] information. It is possible to
[substitute in a custom
implementation][manual:binding-specifications-substitute] of that datatype that
is portable. This manual section includes guidelines for writing portable types.

> [!IMPORTANT]
> Even with portable types, function bindings are currently not portable
> due to [FFI types][manual:portability-ffi-types]. We are planning on
> improving this. See [issue #1748][issue-1748].

This manual section is not necessarily exhaustive. If there is missing
information that you need, please let us know by creating an issue on the [issue
tracker][github:issue-tracker].

#### `Storable`

Portable types should get a `Storable` instance (if applicable) that uses
platform-dependent offsets, sizes, and alignments.

Consider this example `struct`:

```c
struct S { int x; int y; };
```

We can insert the right memory layout values at compile time using `hsc2hs`, for example:

```hs
data S = S { x :: CInt, y :: CInt }
instance Storable S where
  sizeOf    _ = #size      struct S
  alignment _ = #alignment struct S

  peek s = do
      x <- (#peek struct S, x) s
      y <- (#peek struct S, y) s
      return S{x, y}

  poke s S{x, y} = do
      (#poke struct S, x) s x
      (#poke struct S, y) s y
```

> [!Note]
> We have ideas about making generated `Storable` instances more portable right
> out of the box. See [issue #1818][issue-1818].

#### `HasFFIType`

Portable types should get a `HasFFIType` instance (if applicable) where the
associated type `FFIType` matches the FFI type that `hs-bindgen` would deduce.

Consider this example `typedef`:

```c
typedef int T;
```

Since the size of `int` is platform-dependent, `FFIType T` should also have a
platform-dependent implementation. For example, if `int` is 32 bits on one
platform, then `FFIType = Int32` should hold, and if `int` is 64 bits, then
`FFIType = Int64` should hold, etc.

#### `IsArray`

Portable types should get an `IsArray` instance (if applicable) where the
associated type `Elem` maps to the array's element type.

Consider these example `typedef`s:

```c
typedef char S;
typedef S T [];
```

`Elem T` should map to `CChar`, or a (custom) type such as a type `S` that has
similar characteristics as `CChar` (e.g., memory layout).



<!-- sources and references -->

[issue-1748]: https://github.com/well-typed/hs-bindgen/issues/1748
[issue-1818]: https://github.com/well-typed/hs-bindgen/issues/1818
[manual:portability-ffi-types]: ../portability.md#functions-and-ffi-types
[manual:portability-memory-layout]: ../portability.md#memory-layout-of-types
[manual:binding-specifications-substitute]: ../binding-specifications.md#substituting-hand-written-types
