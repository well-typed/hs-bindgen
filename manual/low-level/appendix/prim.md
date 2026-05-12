# The `Prim` class

The `Prim` class is used to implement arrays using a packed memory
representation, using memory on the Haskell heap (unlike _storable_ arrays,
which use memory on the _C_ heap). In `hs-bindgen` we can derive `Prim`
instances, but _only_ using `newtype` deriving; we do _not_ offer general `Prim`
instances for composite types such as structs. In this document we explain why.

## `Storable` vs `Prim`

Superficially `Storable` and `Prim` are pretty similar classes:

```hs
class Storable a where
 sizeOf    :: a -> Int
 alignment :: a -> Int

 -- Read from and write to C-managed memory
 peek :: Ptr a      -> IO a
 poke :: Ptr a -> a -> IO ()
```

vs

```hs
class Prim a where
  -- C side requirements
  sizeOf#    :: a -> Int#
  alignment# :: a -> Int#

  -- Read from and write to Haskell-managed memory
  -- You probably only want to do this for /pinned/ memory
  indexOffAddr# :: Addr# -> Int# -> a
  readOffAddr#  :: Addr# -> Int# -> State# s -> (# State# s, a #)
  writeOffAddr# :: Addr# -> Int# -> a -> State# s -> State# s

  -- Read from and write to (Mutable)ByteArray# (may or may not be pinned)
  indexByteArray# :: ByteArray# -> Int# -> a
  readByteArray#  :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  writeByteArray# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
```

The critical difference (from our perspective here) is that `Storable` uses
primitives that use byte-indexing (`Ptr`), whereas the `Prim` `ByteArray#` API
uses functions that are _element_-indexed.

## Instances for composite types

Suppose we have a C object with two fields and the following memory layout in C:

```
    <2 bytes>                <4 bytes>
+---------------+------------------------------+
| x :: uint16_t |        y :: uint32_t         |
+---------------+------------------------------+
```

(Let's for the sake of simplicity say that we are on a platform without
memory alignment requirements; it doesn't really matter for our purposes here.)

The offset (the `Int#` argument) to the core `Prim` functions is always in terms
of elements, rather than in bytes, severely limiting our options for giving
instances of this class. Specifically, we need to decide on a layout of an array
of these objects. The memory layout we choose does not need to match the C
memory layout at all; after all, `Storable` is for dealing with memory on the C
heap, `Prim` is for dealing with memory on the Haskell heap. However, it seems
no suitable order layout exists; we will see some non-options in this section.

_Row-order, no padding_

One natural candidate might be

```
  0                 1                2
++-----+---------++-----+---------++-----+---------++
|| X X | Y Y Y Y || X X | Y Y Y Y || X X | Y Y Y Y ||
++-----+---------++-----+---------++-----+---------++
```

but this does not work: the only way we have to read `YYYY` elements is by
using `indexByteArray#`, and we cannot pretend that this is an array of `YYYY`
(for example, the first `YYYY` element would not be readable).

_Column-order, no padding_

Column-order would look like

```
   0     1     2      0         1         2
++-----+-----+-----++---------+---------+---------++
|| X X | X X | X X || Y Y Y Y | Y Y Y Y | Y Y Y Y ||
++-----+-----+-----++---------+---------+---------++
```

and suffers from the same problem.

_Column-order, with padding_

We could try adding some padding:

```
   0     1     2            0         1         2
++-----+-----+-----+-----++---------+---------+---------++
|| X X | X X | X X | . . || Y Y Y Y | Y Y Y Y | Y Y Y Y ||
++-----+-----+-----+-----++---------+---------+---------++
```

This _would_ work, except for the fact that `indexByteArray#` does not tell us
how many elements there are in total, and so we have no way to compute the
"virtual offset" of the first `YYYY` element.

_Row-order, with padding_

This could in principle work:

```
  0                      1                      2
++-----+-----+---------++-----+-----+---------++-----+-----+---------++
|| X X | . . | Y Y Y Y || X X | . . | Y Y Y Y || X X | . . | Y Y Y Y ||
++-----+-----+---------++-----+-----+---------++-----+-----+---------++
  "0"          "1"       "2"          "3"        "4"         "5"
  <-4bytes->  <-4bytes-> <-4bytes->  <-4bytes-> <-4bytes->  <-4bytes->
```

The "virtual offstets" for the `YYYY` elements (where we pretend this is an
array of YYYY elements only) are shown in quotes underneath the diagram. That
is, to read the three `YYYY` elements, we'd need virtual indices 1, 3 and 5:

```
++-----+-----+---------++-----+-----+---------++-----+-----+---------++
|| . . | . . | Y Y Y Y || . . | . . | Y Y Y Y || . . | . . | Y Y Y Y ||
++-----+-----+---------++-----+-----+---------++-----+-----+---------++
               ^                      ^                      ^
               "1"                    "3"                    "5"
```

This could however be quite wasteful: the elements would need to be aligned at
offsets that are multiples _of their size_ (note that this is quite different
from the padding that the C compiler needs to insert to satisfy _alignment_
requirements).

## (Non-) Workarounds

What we really need is an API that deals in terms of byte offsets rather than
element indices, like `Storable` has.

* While `Prim` offers an `Addr#`-based API, this is not usable for non-pinned
  memory.
* For the IO-based operations `readByteArray#` we could in principle make a copy
  of the relevant slice of the `ByteArray#` (this is a byte-indexed operation)
  and then index fields from that, at the cost of that extra copy. However,
  this is not possible for the pure operation `indexByteArray#` (unless we
  also use `unsafePerformIO`).
