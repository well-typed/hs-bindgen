# Using the low-level interface

## Introduction

### Structs

Consider the following C declarations:

```c
typedef struct triple {
    int a;
    int b;
    int c;
} triple;

void mk_triple(int a, int b, int c, triple* triple);
```

When we run this through `hs-bindgen`, we get[^1]

```haskell
foreign import capi safe "no_external_headers.h mk_triple"
  mk_triple :: CInt -> CInt -> CInt -> Ptr Triple -> IO ()

data Triple = Triple
  { triple_a :: CInt
  , triple_b :: CInt
  , triple_c :: CInt
  }

instance Storable Triple where
  -- implementation omitted for brevity

deriving stock instance Show Triple
deriving stock instance Eq   Triple
```

Which we might use as follows:

```haskell
mkTriple :: Int -> Int -> Int -> Triple
mkTriple a b c = unsafePerformIO $
    alloca $ \ptr -> do
      mk_triple (fromIntegral a) (fromIntegral b) (fromIntegral c) ptr
      peek ptr
```

Conceivably a more high-level API like this could be constructed automatically,
but this is be the responsibility of the `hs-bindgen` _high-level_ interface.

### Enums

The following C code

```c
typedef enum index {
    A,
    B,
    C
} index;

int index_triple(triple* triple, index ix);
```

results in

```haskell
foreign import capi safe "no_external_headers.h index_triple"
  index_triple :: Ptr Triple -> Index -> IO CInt

newtype Index = Index
  { unIndex :: CUInt
  }

instance Storable Index where
  -- implementation omitted for brevity

deriving stock   instance Show Index
deriving stock   instance Read Index
deriving stock   instance Eq   Index
deriving stock   instance Ord  Index
deriving newtype instance Enum Index

pattern A, B, C :: Index
pattern A = Index 0
pattern B = Index 1
pattern C = Index 2
```

Example usage:

```haskell
indexTriple :: Triple -> Index -> Int
indexTriple triple ix = unsafePerformIO $
    with triple $ \ptr -> fromIntegral <$> index_triple ptr ix
```

The reasont that `Index` is defined as a `newtype` around `CUInt` rather than
a Haskell ADT is that a C enum declaration only defines _values_; it does not
limit the range of the corresponding type. For the same reason we do not derive
`Bounded` for `Index`, and the patterns `A`, `B` and `C` are _not_ declared
[complete][ghc-manual:complete]. Generating a Haskell ADT to correspond to
this enum will be the responsibility of the `hs-bindgen` high-level interface.

### Typedefs

C `typedef`s are translated to Haskell `newtype`s:

```c
typedef int sum;

sum sum_triple(triple* triple);
```

becomes

```haskell
newtype Sum = Sum
  { unSum :: CInt
  }

deriving stock instance Eq   Sum
deriving stock instance Ord  Sum
deriving stock instance Read Sum
deriving stock instance Show Sum

deriving newtype instance Storable   Sum
deriving newtype instance Enum       Sum
deriving newtype instance Ix.Ix      Sum
deriving newtype instance Bounded    Sum
deriving newtype instance Bits.Bits  Sum
deriving newtype instance FiniteBits Sum
deriving newtype instance Integral   Sum
deriving newtype instance Num        Sum
deriving newtype instance Real       Sum

foreign import capi safe "no_external_headers.h sum_triple"
  sum_triple :: Ptr Triple -> IO Sum
```

We use `newtype` instead of `type` because C typedefs often contain semantic
information (e.g., `clock_t`). In addition, we might want different type class
instances; by default we inherit the instances that are available for the
underlying type, but this can be overridden.

### Macros

For macros we generate different Haskell code depending on the kind of macro.

#### Values

Value definitions such as

```c
#define FIELD_OFFSET 4
#define EPSILON 0.1
```

become corresponding value definitions in Haskell:

```haskell
fIELD_OFFSET :: FC.CInt
fIELD_OFFSET = 4

ePSILON :: FC.CDouble
ePSILON = 0.1
```

#### Functions

A macro such as `FIELD_OFFSET` is typically used as a pointer offset; for
example, it might be used like

```haskell
poke (plusPtr ptr (fromIntegral fIELD_OFFSET)) (1234 :: Word32)
```

Sometimes however such field offsets are not defined as offsets but rather as
macro functions:

```haskell
#define PTR_TO_FIELD(ptr) ptr + 4
```

C macros don't have type annotations, and we cannot tell from just looking at
the macro body how it's intended to be used. Therefore `hs-bindgen` applies a
type inference algorithm and generates a Haskell function with a very general
type:

```haskell
pTR_TO_FIELD :: CExpr.Add a CInt => a -> CExpr.AddRes a CInt
```

`Add` and `AddRes` are defined in a `hs-bindgen` companion library called
`c-expr`, which essentially allows us to mirror C expressions in Haskell,
respecting the typing rules for C. Two examples:

* If we instantiate `a` to `(Ptr x)`, adding `CInt` (offset) to a pointer, the
  result will also be `(Ptr x)`
* If we instantiate `a` to `CLong`, adding `CInt` and `CLong`, the result will
  be `CLong`.

#### Types

Macros can also be used to define types:

```c
#define YEAR  int
#define MONTH int
#define DAY   int

typedef struct date {
    YEAR  year;
    MONTH month;
    DAY   day;
} date;

YEAR getYear(date* d);
```

This results in

```haskell
newtype YEAR = YEAR -- and similarly for MONTH and DAY
  { unYEAR :: CInt
  }

deriving newtype instance F.Storable YEAR
deriving stock instance Eq YEAR
-- and more instances

data Date = Date
  { date_year :: YEAR
  , date_month :: MONTH
  , date_day :: DAY
  }

foreign import capi safe "no_external_headers.h getYear"
  getYear :: Ptr Date -> IO FC.CInt
```

TODO: This type of `getYear` is wrong (#399).


[^1]: Slightly edited for layout.

[ghc-manual:complete]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#complete-pragmas
