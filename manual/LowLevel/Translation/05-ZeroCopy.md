# Zero-copy API

The zero-copy API allows C objects to be manipulated through pointers without
marshalling the entire object to Haskell and then back to C. The term *zero-copy
API* colloquially refers to both the code that `hs-bindgen` generates to do this
type of pointer manipulation, and also the infrastructure in
`hs-bindgen-runtime` that supports it.

## Introduction

Suppose we want to override one value deeply nested in some C data structure. We
could use the `Storable` instances to `peek` the value, then override the
appropriate field, and finally `poke` the updated value:

```hs
introduction :: IO ()
introduction =
    alloca $ \(rectPtr :: Ptr Gen.Rectangle) -> do
      rect <- peek rectPtr
      poke rectPtr $ rect & #rectangle_topleft % #point_x .~ 2
```

(The use of lenses here is optional of course.)

However, it may well be undesirable to marshall the entire structure back and
forth merely to change a single value. This is why `hs-bindgen` also generates
`HasField` instances for pointers, so that record dot syntax can be used to
index C structures. We can update the same field as before without marshalling
the entire structure as follows:

```hs
      poke rectPtr.rectangle_topleft.point_x 2
```

`HasField` instances are implemented under the hood with a custom class
infrastructure. If you prefer to avoid record-dot syntax, you can use the
``HsBindgen.Runtime.HasCField` infrastructure directly.

The zero-copy API also supports zero-copy pointer manipulation of C objects
other than just structures. The supported operations are:

* Indexing of union fields through a pointer
* Indexing of bit-fields (for both structures and unions) through a pointer
* Erasing `newtype` sugar (introduced by `typedef`s or macro-defined types) from
  a pointer type
* Convering a pointer to an array to a pointer to the array's first element
