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

  toCEnum        :: CEnumZ a -> a
  fromCEnum      :: a -> CEnumZ a
  declaredValues :: proxy a -> DeclaredValues a
```

The generated instance for `Index` is given by

```haskell
instance CEnum Index where
  type CEnumZ Index = FC.CUInt

  toCEnum        = Index
  fromCEnum      = un_Index
  declaredValues = declaredValuesFromList [
      (0, NonEmpty.singleton "A")
    , (1, NonEmpty.singleton "B")
    , (2, NonEmpty.singleton "C")
    ]
```

Functions `toCEnum` and `fromCEnum` correspond directly to `toEnum` and
`fromEnum`, but unlike `Enum` this makes no assumptions about successors or
predecessors. Instead we only declare a set of known values, and the names
assigned to those values.

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

The `Show` we derive for enums also makes use of the `CENum` class: if a value
appears in `declaredValues`, then we use the name declared. This is a
law-abiding `Show` instance (that is, this generates valid Haskell) due to the
patterns we generate for enums. For example, suppose we do

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

This is not _always_ optimal; for example, [`libclang`][libclang], the C library
that provides an API for the `clang` C compiler (which `hs-bindgen` itself
heavily relies upon) defines enums of this general shape (this is an extract
of [`CXCursor`][libclang:CXCursor]):

```c
enum CXCursorKind {
  CXCursor_FirstExpr        = 100,
  CXCursor_UnexposedExpr    = 100,
  CXCursor_DeclRefExpr      = 101,
  CXCursor_MemberRefExpr    = 102,
  CXCursor_CallExpr         = 103,
  // .. many expressions omitted ..
  CXCursor_PackIndexingExpr = 156,
  CXCursor_LastExpr = CXCursor_PackIndexingExpr,

  CXCursor_FirstStmt              = 200,
  CXCursor_UnexposedStmt          = 200,
  CXCursor_LabelStmt              = 201,
  CXCursor_CompoundStmt           = 202,
  CXCursor_CaseStmt               = 203,
  // .. many statements omitted ..
  CXCursor_OpenACCUpdateConstruct = 331,
  CXCursor_LastStmt = CXCursor_OpenACCUpdateConstruct,
};
```

This enum is divided into "sections" (statements, expressions, ..), and each
section is delimited by two special "first" and "last" values. The default
`Show` instance generated by `hs-bindgen` will then print value "100" as
`CXCursor_FirstExpr`, where `CXCursor_UnexposedExpr` would probably have been
more helpful.

> [!NOTE]
> It is not yet possible to prevent `hs-bindgen` from generating instances.
> https://github.com/well-typed/hs-bindgen/issues/307


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




[libclang]: https://clang.llvm.org/doxygen/group__CINDEX.html
[libclang:CXCursor]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013
