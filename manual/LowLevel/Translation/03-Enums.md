# Enums

## Different flavours of `enum`

### Simple `enum`s

We already saw the simplest form of `enum` in the
[Introduction](Introduction.md): given

```c
typedef enum index {
    A,
    B,
    C
} index;
```

we generate

```haskell
newtype Index = Index {
    un_Index :: CUInt
  }

pattern A :: Index
pattern B :: Index
pattern C :: Index

pattern A = Index 0
pattern B = Index 1
pattern C = Index 2
```

These patterns are not declared `COMPLETE`, because C `enum`s merely declare
values, they do not restrict the range.

### User-defined ranges

Enums can be given custom ranges, either consecutive as in

```c
enum signal {
  start = 1,
  pause,
  resume,
  stop
};
```

or non-consecutive as in

```c
enum HTTP_status {
  ok           = 200,
  moved        = 301,
  bad_request  = 400,
  unauthorized = 401,
  not_found    = 404
};
```

This does not affect the type definition, but does (obviously) affect the
patterns, as well as the `CEnum` instance (see below).

### Different types

In the translation of `index` we used `CUInt`, corresponding to `unsingned int`
in C. This is the default, but sometimes the C compiler will choose a different
type, and `hs-bindgen` will follow suit. For example, for

```c
enum result {
  failed       = -1,
  success      = 0,
  postponed    = 1,
  already_done = 2
};
```

we use `CInt` instead (corresponding to `signed int`):

```haskell
newtype Result = Result {
    un_Result :: CInt
  }
```

and for

```c
enum vote {
  infavour,
  against,
  abstain
} __attribute__((packed));
```

we use `CSChar` (corresponding to `signed char`):

```haskell
newtype Vote = Vote {
    un_Vote :: CSChar
  }
```

## The `CEnum` class

At first glance it would seem obvious that the Haskell type defined for C enums
should be given an `Enum` instance; if `Enum` corresponds to enumeration types,
then surely C `enum`s qualify. However, as mentioned above, C enums merely
identify some values; they do not restrict the range of the corresponding type.
Take `HTTP_status` (see above), and consider  `Moved` (with value 301); should
`succ Moved` be `Bad_request` (value 400), the next _declared_ value, or should
it be value 302? `Bounded` is problematic for the same reason: should `minBound`
be `ok` (value 200) or 0 (`minBound` for `CUInt`)?

The `hs-bindgen-runtime` library therefore defines an alternative class called
`CENum`, the most important members of which are:

```haskell
class Integral (CEnumZ a) => CEnum a where
  type CEnumZ a -- ^ The underlying type

  toCEnum   :: CEnumZ a -> a
  fromCEnum :: a -> CEnumZ a

  declaredValues  :: proxy a -> DeclaredValues a
  showsUndeclared :: proxy a -> Int -> CEnumZ a -> ShowS
  readPrecUndeclared :: ReadPrec a
```

The generated instance for `Index` is given by

```haskell
instance CEnum Index where
  type CEnumZ Index = FC.CUInt

  toCEnum   = Index
  fromCEnum = un_Index

  declaredValues _ = declaredValuesFromList [
      (0, NonEmpty.singleton "A")
    , (1, NonEmpty.singleton "B")
    , (2, NonEmpty.singleton "C")
    ]
  showsUndeclared = showsWrappedUndeclared "Index"
  readPrecundeclared = readPrecWrappedUndeclared "Index"
```

Functions `toCEnum` and `fromCEnum` correspond directly to `toEnum` and
`fromEnum`, but unlike `Enum` this makes no assumptions about successors or
predecessors. Instead we only declare a set of known values, and the names
assigned to those values. We will come back to `showsUndeclared` and
`readPrecUndeclared` when we discuss `Show` and `Read` instances, below.

In addition, `hs-bindgen-runtime` defines a class `SequentialCEnum`, which
corresponds to `Bounded`:

```haskell
class CEnum a => SequentialCEnum a where
  minDeclaredValue :: a
  maxDeclaredValue :: a
```

The generated instance for `Index` is as expected:

```haskell
instance SequentialCEnum Index where
  minDeclaredValue = A
  maxDeclaredValue = C
```

## Show instance

The `Show` we derive for enums also makes use of the `CENum` class:

```haskell
instance Show HTTP_status where
  showsPrec = showsCEnum
```

where

```haskell
showsCEnum :: forall a. CEnum a => Int -> a -> ShowS
```

makes use of the names in `declaredValues` whenever possible, and also uses the
class method `showsUndeclared` to show undeclared values. This is a law-abiding
`Show` instance in that it generates valid Haskell, due to the patterns we
generate for enums. For example, suppose we do

```haskell
deriving newtype instance Bounded HTTP_status
```

then

```haskell
print [Ok, minBound]
```

results in

```haskell
[Ok,HTTP_status 0]
```

The value of `HTTP_status 0` here comes from the `Bounded` instance inherited
from the underlying `CUInt` (due to the `newtype` deriving clause). Since it has
no pattern associated with it, `showsCEnum` falls back on `showsUndeclared`,
which by default (`showsWrappedUndeclared`) uses the newtype constructor name
`HTTP_status` instead.

### Overrides

The derived `Show` is not _always_ optimal; for example, [`libclang`][libclang],
the C library that provides an API for the `clang` C compiler (which
`hs-bindgen` itself heavily relies upon) defines enums of this general shape
(this is an extract of [`CXCursor`][libclang:CXCursor]):

```c
enum CXCursorKind {
  CXCursor_FirstExpr        = 100,
  CXCursor_UnexposedExpr    = 100,
  CXCursor_DeclRefExpr      = 101,
  CXCursor_MemberRefExpr    = 102,
  // .. many expressions omitted ..
  CXCursor_PackIndexingExpr = 156,
  CXCursor_LastExpr = CXCursor_PackIndexingExpr,

  CXCursor_FirstStmt              = 200,
  CXCursor_UnexposedStmt          = 200,
  CXCursor_LabelStmt              = 201,
  CXCursor_CompoundStmt           = 202,
  // .. many statements omitted ..
  CXCursor_OpenACCUpdateConstruct = 331,
  CXCursor_LastStmt = CXCursor_OpenACCUpdateConstruct,
};
```

This enum is divided into "sections" (statements, expressions, ..), and each
section is delimited by two special "first" and "last" values. The default
`Show` instance generated by `hs-bindgen` will then print value "100" as
`CXCursor_FirstExpr`, where `CXCursor_UnexposedExpr` would probably have been
more helpful. Fortunately, it is not difficult to add some overrides:

```haskell
showCursorKind :: CXCursorKind -> String
showCursorKind = \case
    CXCursor_UnexposedExpr -> "CXCursor_UnexposedExpr"
    CXCursor_UnexposedStmt -> "CXCursor_UnexposedStmt"
    kind -> show kind
```

We also provide a helper function `showCEnum` for C enums without a specialized
`Show` instance:

```haskell
showCEnum :: forall a. CEnum a => a -> String
```

> [!NOTE]
> It is not yet possible to prevent `hs-bindgen` from generating instances.
> <https://github.com/well-typed/hs-bindgen/issues/307>

## Read instance

Similar to the `Show` instance, we define the `Read` instance as

```haskell
instance Read HTTP_status where
  readPrec = readPrecCEnum
```

where

```haskell
readPrecCEnum :: forall a. (CEnum a, Read (CEnumZ a)) => ReadPrec a
```

`readPrecCEnum` uses the class method `readPrecUndeclared` to read undeclared
values. We also define default implementations for `readList`, and
`readListPrec` as detailed in
[Text.Read](https://hackage.haskell.org/package/base/docs/Text-Read.html#t:Read).

The function `readPrecCEnum` parses declared and undeclared values. For example,

```haskell
read "Ok" :: HTTP_status == read "HTTP_status 200" :: HTTP_status
```

evaluates to `True`.

### Overrides

Also the `Read`-related functions can be overridden. For example, the derived
`Read` instances inherit the `Read`-related properties of the underlying
integral type. In particular, we parse negative integers, even when the integral
type is unsigned:

```haskell
read "Index (-1)" :: Index -- Evaluates to 'Index 4294967295'
```

We can ensure the value is within a given bound by providing a
`readEitherIndexWith` function like so:

```haskell
readEitherIndexWith :: CUInt -> String -> Either String Index
readEitherIndexWith upperBound x = case readEither x of
  Right (Index v) | v > upperBound -> Left $ "index out of bounds: " <> show v
  other                            -> other
```

We also provide a helper function for C enums without a specialized `Read`
instance:

```haskell
readEitherCEnum :: forall a. CEnum a => String -> a
```

> [!NOTE]
> It is not yet possible to prevent `hs-bindgen` from generating instances.
> <https://github.com/well-typed/hs-bindgen/issues/307>

## Deriving-via support

If for a given application an `Enum` or a `Bounded` instance _is_ appropriate,
users can derive those instances in one of two ways. First, it is possible to
use `newtype` deriving, to get the `Enum`/`Bounded` instance corresponding to
the underlying type; we saw an example above with the `Bounded` instance for
`HTTP_status`. In addition, the `hs-bindgen-runtime` provides
`AsCEnum` and `AsSequentialCEnum` newtype wrappers intended for use with
deriving-via. For example, if we use

```haskell
deriving via AsSequentialCEnum Vote instance Enum    Vote
deriving via AsSequentialCEnum Vote instance Bounded Vote
```

then running

```haskell
putStrLn $ "Possible votes: " ++ show ([minBound .. maxBound] :: [Vote])
```

will result in

```
Possible votes: [Infavour,Against,Abstain]
```

## Enums and ordering

Finally, some thoughts on enums and ordering.

### `fromCEnum`

The `CENum` class insists if `a` has both a `CEnum` instance and an `Ord`
instance, the `Ord` instance on `a` must be compatible with the `Ord` instance
on the underlying integral type:

```
(x <= y)   if and only if  (fromCEnum x <= fromCEnum y)
```

This is true for the code we generate: the Haskell type is a newtype wrapper
around the integral type, `fromCEnum` just removes the newtype wrapper (in other
words, it's the identity), and the `Ord` instance for the newtype is derived
from the underlying type.

### `fromEnum`

If we remove error checking from the definition of `toEnum` and `fromEnum` for
both the `AsCEnum` and the `AsSequentialCEnum` newtype wrappers we get

```haskell
instance CEnum a => Enum (AsCEnum a) where
  toEnum   = coerce       . toCENum   . fromIntegral
  fromEnum = fromIntegral . fromCEnum . coerce
```

Provided that `fromIntegral` doesn't do anything very strange, this means that
we must also have

```
(x <= y)   if and only if  (fromEnum x <= fromEnum y)
```

### `succ`/`pred`

For _sequential_ enums we have

```haskell
instance SequentialCEnum a => Enum (AsSequentialCEnum a) where
  succ = coerce . toCEnum . succ . fromCEnum . coerce
  pred = coerce . toCEnum . pred . fromCEnum . coerce
```

In other words, `succ` and `pred` are just inherited from the underlying
integral type, so that we have for all `x`

```
pred x < x < succ x
```

For non-sequential enums the definition is more complicated (we look for the
next or previous _declared_ value), but we do preserve this property. This may
sometimes result in definitions that are perhaps slightly surprising. For
example, consider this C definition:

```c
enum descending {
  X       = 100,
  Y       = 99,
  Y_alias = 99,
  Z       = 98,
};
```

Then using the `hs-bindgen` generated translation of `descending` we have

```haskell
succ Y == X
pred Y == Z
```

This may go counter to the intuition we might have as a Haskell programmer, but
the alternative (`succ Y == Z`, following the order of the declared names in the
C `enum`) would break one of the two properties we stated above:

1. If the `Ord` instance for (the Haskell type) `Descending` is inherited from
   the underlying `CUInt`, then `succ Y < Y`.
2. If instead the `Ord` instance for `Descending` is such that `Z > Y`,
   we still have `fromEnum Z < fromEnum Y`.

We feel that either of these two options would ultimately result in more
confusion rather than less.

[libclang]: https://clang.llvm.org/doxygen/group__CINDEX.html
[libclang:CXCursor]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013
