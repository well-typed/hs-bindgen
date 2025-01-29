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

data Triple = Triple {
    triple_a :: CInt
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

> [!NOTE]
> Conceivably a more high-level API like this could be constructed
> automatically, but this will be the responsibility of the `hs-bindgen`
> _high-level_ interface.

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

newtype Index = Index {
    unIndex :: CUInt
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

The reason that `Index` is defined as a `newtype` around `CUInt` rather than
a Haskell ADT is that a C enum declaration only defines _values_; it does not
limit the range of the corresponding type. For the same reason we do not derive
`Bounded` for `Index`, and the patterns `A`, `B` and `C` are _not_ declared
[complete][ghc-manual:complete].

> [!NOTE]
> Generating a Haskell ADT to correspond to this enum will be the responsibility
> of the `hs-bindgen` high-level interface.

### Typedefs

C `typedef`s are translated to Haskell `newtype`s:

```c
typedef int sum;

sum sum_triple(triple* triple);
```

becomes

```haskell
newtype Sum = Sum {
    unSum :: CInt
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
fIELD_OFFSET :: CInt
fIELD_OFFSET = (4 :: CInt)

ePSILON :: CDouble
ePSILON = (0.1 :: CDouble)
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
the macro body how it's intended to be used. Therefore `hs-bindgen` uses type
inference to generate a Haskel function with the most general type possible:

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

This means that we can `peek` the value from the `poke` example above using

```haskell
peek (pTR_TO_FIELD ptr)
```

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
-- .. similarly for MONTH and DAY
newtype YEAR = YEAR {
    unYEAR :: CInt
  }

deriving newtype instance Storable YEAR
deriving stock   instance Eq       YEAR
-- and more instances

data Date = Date {
    date_year  :: YEAR
  , date_month :: MONTH
  , date_day   :: DAY
  }

foreign import capi safe "no_external_headers.h getYear"
  getYear :: Ptr Date -> IO CInt
```

TODO: This type of `getYear` is wrong (#399).

## Invoking

### CLI

### Library

## Naming

> [!NOTE]
> Many aspects of naming are configurable by providing an alternative
> implementation of the `NameMangler`. Currently this requires using
> `hs-bindgen` as a library; providing a syntax for modifying some aspects of
> name mangling through the CLI is future work. Here we discuss the defaults.

When generating Haskell bindings for C code, we need to choose Haskell names.
The default choice in `hs-bindgen` is to stick as closely as possible to the
names in the C code, whilst generating valid Haskell identifiers and avoiding
name clashes that might arise from different name spacing rules.

* Any characters that aren't valid in Haskell identifiers (that is, anything
  other than alphanumeric characters or underscore) are escaped.
* If the first character is a letter, then it is changed to uppercase or
  lowercase as appropriate. If the first character is not a letter, the name is
  prefixed with a `C`; one important example of this is C identifiers that start
  with an underscore.
* If this results in a reserved name (a Haskell keyword), a single quote is
  added to the name.

### Structs

For C `struct` definitions:

* The name of the (Haskell) constructor is identical to the name of the type.
* To avoid name clashes, record fields are prefixed with the type name.
* Nested struct declarations are treated the same as top-level declarations.

For example,

```c
struct odd {
  int value;
  struct even {
    double value;
    struct odd *next;
  } *next;
};
```

results in

```haskell
data Odd = Odd {
    odd_value :: CInt
  , odd_next  :: Ptr Even
  }

data Even = Even {
    even_value :: CDouble
  , even_next  :: Ptr Odd
  }
```

For _anonymous_ nested struct declarations we combine the name of the enclosing
struct with the field name:

```c
struct S1 {
  struct {
    int a;
    int b;
  } c;

  int d;
};
```

results in

```haskell
data S1 = S1 {
    s1_c :: S1_c
  , s1_d :: CInt
  }

data S1_c = S1_c {
    s1_c_a :: CInt
  , s1_c_b :: CInt
  }
```

TODO: This now covers most (all?) of `mangleTypeConstrName` and
`mangleConstrName`, but not yet `mangleVarName`.


















## Structs

In this section we will consider the details of the translation of C structs.






### Nested structs

### Bitfields

### Flexible array members (FLAMs)

### Opaque structs




[^1]: Slightly edited for layout.

[ghc-manual:complete]: https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#complete-pragmas
