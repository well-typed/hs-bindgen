# Introduction to the low-level API

## Structs

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
$(addCSource "... void hs_bindgen_532be147b0ffdb88 ...")
foreign import ccall safe "hs_bindgen_532be147b0ffdb88"
  mk_triple :: CInt -> CInt -> CInt -> Ptr Triple -> IO ()

data Triple = Triple {
    triple_a :: CInt
  , triple_b :: CInt
  , triple_c :: CInt
  }
  deriving stock (Eq, Show)

instance Storable Triple where
  -- implementation omitted for brevity
```

Which we might use as follows:

```haskell
mkTriple :: Int -> Int -> Int -> Triple
mkTriple a b c = unsafePerformIO $
    alloca $ \ptr -> do
      mk_triple (fromIntegral a) (fromIntegral b) (fromIntegral c) ptr
      peek ptr
```

> [!NOTE]
> `addCSource` is a Template Haskell splice that adds the C source code at
> compile time, allowing `hs-bindgen` to provide C-native functionality and
> directly interface with the original C code.

> [!NOTE]
> Conceivably, a more high-level API like this could be constructed
> automatically, but this will be the responsibility of the `hs-bindgen`
> _high-level_ interface.

## Enums

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
$(addCSource "... signed int hs_bindgen_c4886273ec5abcf5 ...")
foreign import ccall safe "hs_bindgen_c4886273ec5abcf5"
  index_triple :: Ptr Triple -> Index -> IO CInt

newtype Index = Index {
    unwrapIndex :: CUInt
  }
  deriving stock (Eq, Ord)

instance Storable        Index where -- ...
instance Show            Index where -- ...
instance Read            Index where -- ...
instance CEnum           Index where -- ...
instance SequentialCEnum Index where -- ...

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

The reason that `Index` is defined as a `newtype` around `CUInt` rather than a
Haskell ADT is that a C `enum` declaration only defines _values_; it does not
limit the range of the corresponding type. For the same reason we do not derive
`Enum` or `Bounded` for `Index`, and the patterns `A`, `B` and `C` are _not_
declared [complete][ghc-manual:complete].

> [!NOTE]
> The `CENum` class will provide an alternative interface to `Enum` and
> `Bounded`.
> <https://github.com/well-typed/hs-bindgen/pull/552>

> [!NOTE]
> Generating a Haskell ADT to correspond to this `enum` will be the
> responsibility of the `hs-bindgen` high-level interface.

## Typedefs

C `typedef`s are translated to Haskell `newtype`s:

```c
typedef int sum;

sum sum_triple(triple* triple);
```

becomes

```haskell
newtype Sum = Sum {
    unwrapSum :: CInt
  }
  deriving stock   (Eq, Ord, Read, Show)
  deriving newtype (Storable, Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

$(addCSource "... sum hs_bindgen_94482b93bc4c5bc9 ...")
foreign import ccall safe "hs_bindgen_94482b93bc4c5bc9"
  sum_triple :: Ptr Triple -> IO Sum
```

We use `newtype` instead of `type` because C typedefs often contain semantic
information (e.g., `clock_t`). In addition, we might want different type class
instances; by default we inherit the instances that are available for the
underlying type, but this can be overridden.

## Macros

For macros we generate different Haskell code depending on the kind of the macro.

### Values

Value definitions such as

```c
#define FIELD_OFFSET 4
#define EPSILON 0.1
```

become corresponding value definitions in Haskell:

```haskell
fIELD_OFFSET :: CInt
fIELD_OFFSET = (4 :: CInt)

ePSILON :: CDouble
ePSILON = (0.1 :: CDouble)
```

### Functions

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

C macros lack type annotations, and we cannot tell from just looking at the
macro body how it is intended to be used. Therefore `hs-bindgen` uses type
inference to generate a Haskel function with the most general type possible:

```haskell
pTR_TO_FIELD :: forall a. (C.Add a) CInt => a -> (C.AddRes a) CInt
```

`Add` and `AddRes` are defined in an `hs-bindgen` companion library called
`c-expr`, which essentially allows us to mirror C expressions in Haskell,
respecting the typing rules for C. Two examples:

* If we instantiate `a` to `(Ptr x)`, adding `CInt` (offset) to a pointer, the
  result will also be `(Ptr x)`
* If we instantiate `a` to `CLong`, adding `CInt` and `CLong`, the result will
  be `CLong`.

This means that we can `peek` the value from the `poke` example above using

```haskell
peek (pTR_TO_FIELD ptr)
```

### Types

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
-- ... similarly for MONTH and DAY
newtype YEAR = YEAR {
    unwrapYEAR :: CInt
  }
  deriving stock   (Eq, Ord, Read, Show)
  deriving newtype (Storable, Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data Date = Date {
    date_year  :: YEAR
  , date_month :: MONTH
  , date_day   :: DAY
  }

$(addCSource "... YEAR hs_bindgen_1c549fc082857e68 ...")
foreign import ccall safe "hs_bindgen_1c549fc082857e68"
  getYear :: Ptr Date -> IO YEAR
```

[^1]: Slightly edited for layout.

[ghc-manual:complete]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#complete-pragmas
