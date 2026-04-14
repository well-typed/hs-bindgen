# Pointer manipulation API

Suppose we want to override one value nested in some C data structure. We could
use the `Storable` instances to `peek` the value, then override the appropriate
field, and finally `poke` the updated value. However, it may be undesirable to
marshall the entire structure back and forth merely to change a single value.

The pointer manipulation API allows a nested value that is stored in a pointer
to be manipulated without marshalling its enclosing data structure(s) to pure
Haskell values and back. The term *pointer manipulation API* colloquially refers
to both the code that `hs-bindgen` generates to do this type of pointer
manipulation, and also the infrastructure in `hs-bindgen-runtime` that supports
it.

Whether or not it is desirable to use the pointer manipulation API depends on
your case. Our recommendation would be to use it if your data structures are
large and you care about performance.

## Feature list

The pointer manipulation API supports:

* Indexing of struct fields through a pointer
* Indexing of union fields through a pointer
* Indexing of bit-fields (for both structs and unions) through a pointer
* Erasing `newtype` sugar (introduced by `typedef`s or macro-defined types) from
  a pointer type
* Convering a pointer to an array to a pointer to the array's first element

## Example

Let's consider an example to illustrate how to use the tools offered by the
pointer manipulation API. We define a `point` struct that is used twice in a
`rectangle` struct[^1]:

```c
struct point {
  int x;
  int y;
};

struct rectangle {
  struct point topleft;
  struct point bottomright
}
```

We generate Haskell bindings for these structs, which look roughly like so
(details omitted):

```hs
data Point = Point
  { point_x :: CInt
  , point_y :: CInt
  }
deriving ... instance Storable Point

data Rectangle = Rectangle
  { rectangle_topleft :: Point
  , rectangle_bottomright :: Point
  }
deriving ... instance Storable Rectangle
```

If we want to modify a `rectangle` that is stored in a pointer, then we could
choose to fully marshall the value to a Haskell value and back. We first read
the value off the pointer, modify it, then write it back[^2]:

```hs
example1 :: Ptr Rectangle -> IO ()
example1 rectPtr = do
      rect <- peek rectPtr
      poke rectPtr $ rect & #rectangle_topleft % #point_x .~ 2
```

In this concrete example it might not be so costly to marshall the full
`rectangle` struct and its two `point` structs, but the cost of full marshalling
scales with the number of fields in the structs, so it is not hard to imagine a
use case where full marshalling is better avoided. Nonetheless, the `rectangle`
example should be sufficient to illustrate how to *use* the pointer manipulation
API.

Since full marshalling may be undesirable, `hs-bindgen` also generates
`HasField` instances for pointers, so that record dot syntax can be used to
index C data structures. These instances roughly look like so (details omitted):

```hs
instance ( ... ) => HasField "point_x"               (Ptr Point)     (Ptr CInt)
instance ( ... ) => HasField "point_y"               (Ptr Point)     (Ptr CInt)
instance ( ... ) => HasField "rectangle_topleft"     (Ptr Rectangle) (Ptr Point)
instance ( ... ) => HasField "rectangle_bottomright" (Ptr Rectangle) (Ptr Point)
```

We can update the same field as before without full marshalling as follows:

```hs
example2 :: Ptr Rectangle -> IO ()
example2 rectPtr = poke rectPtr.rectangle_topleft.point_x 2
```

`HasField` instances are implemented under the hood with a custom class
infrastructure. If you prefer to avoid record-dot syntax, you can either:

* Use the `getField` function from the `HasField` class, or
* Use the `HsBindgen.Runtime.HasCField` infrastructure directly.

<!-- footnotes -->

[^1]: Though the example only uses structs, the pointer manipulation API
    supports many other types and other use cases as well. See the [Feature list
    section](#feature-list) for all supported features.

[^2]: The use of lenses here is optional.
