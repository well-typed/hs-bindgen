## Table of contents

1. [Problem and context](#1-problem-and-context)
2. [Some C example patterns](#2-some-c-example-patterns)
3. [State of the art](#3-state-of-the-art)
4. [What `hs-bindgen` ships today](#4-what-hs-bindgen-ships-today)
5. [Servant's type-class elaboration mechanism](#5-servants-type-class-elaboration-mechanism)
6. [Design](#6-design)

<a id="1-problem-and-context"></a>

## 1. Problem and context

### 1.1 What `hs-bindgen` does today

`hs-bindgen` reads a C header and produces a **low-level** Haskell binding
that mirrors the C API one-for-one. Pointers stay as `Ptr`, integers as
`CInt`, booleans as `CBool`, strings as `Ptr CChar` (or `PtrConst CChar`
for `const char *`). The runtime support library
(`hs-bindgen-runtime`) provides the glue: `IsArray`, `IncompleteArray`,
`PtrConst`, `CEnum`, `HasCField`, `HasFFIType`, `ToFunPtr`,
`FromFunPtr`, etc.

However, what most users want is a Haskell-native API where strings are
`String` or `ByteString`, bools are `Bool`, nullable pointers are `Maybe`,
arrays are `Vector` or `IncompleteArray` with an obvious value-level shape,
and resource lifetimes are managed by `bracket`-shaped combinators. Today,
users get there by hand-writing a wrapper module on top of the generated
bindings.

This document is about Milestone 2.5 (`manual/roadmap.md:76-83`): a small,
compositional, type-class-based mechanism that turns a high-level wrapper
module from a 320-line `withCString` / `alloca` / `peek` mess into a file that
specifies only the API that the user wants to see.

<a id="2-some-c-example-patterns"></a>

## 2. Some C example patterns

Four C functions, in increasing complexity. For each: the C signature, the
low-level binding `hs-bindgen` generates today, the high-level wrapper a user
must currently write, and the wrapper they would write if a small library of
compositional helpers existed.

The point of this section is to show the shape we are aiming for. A clean
Haskell signature, with the conversions either inferred by the library or
assembled from short, compositional pieces.

### 2.1 `strlen`: one Haskell arg, one Haskell result

The minimal case.

```c
size_t strlen(const char *s);
```

What `hs-bindgen` generates:

```haskell
strlen :: PtrConst CChar -> IO CSize
```

What a user writes today:

```haskell
hsStrlen :: String -> IO Int
hsStrlen s = withCString s $ \p ->
               fromIntegral <$> strlen (PtrConst.unsafeFromPtr p)
```

What we want them to write:

```haskell
hsStrlen :: String -> IO Int
hsStrlen = hl strlen
```

The conversions are obvious from the types alone:

- `String ↔ Ptr CChar` (with `withCString`-bracket semantics);
- `CSize ↔ Int` (with `fromIntegral`).

A library should be able to discover them and apply them.

### 2.2 `do_thing(str, len)`: one Haskell arg, two C arguments

The pattern where one Haskell value naturally expands into a pair of C
arguments. The `c2hs` documentation calls this the `&` modifier.

```c
int do_thing(const char *str, int len);
```

What `hs-bindgen` generates:

```haskell
do_thing :: PtrConst CChar -> CInt -> IO CInt
```

What a user writes today:

```haskell
hsDoThing :: ByteString -> IO Int
hsDoThing bs =
  BS.useAsCStringLen bs $ \(p, n) ->
    fromIntegral <$> do_thing (PtrConst.unsafeFromPtr p) (fromIntegral n)
```

What we want them to write:

```haskell
hsDoThing :: ByteString -> IO Int
hsDoThing = hl do_thing
```

The library needs to know that a `ByteString` corresponds to *two* consecutive
C slots `(PtrConst CChar, CSize)`, and `useAsCStringLen` is the bracket that
produces them.

### 2.3 `parse_int(s, out)`: input arg + out parameter

```c
int parse_int(const char *s, int *out);
```

What `hs-bindgen` generates:

```haskell
parse_int :: PtrConst CChar -> Ptr CInt -> IO CInt
```

What a user writes today:

```haskell
hsParseInt :: String -> IO (Int, Int)  -- (parsed value, status)
hsParseInt s =
  withCString s $ \pStr ->
    alloca $ \pOut -> do
      status <- parse_int (PtrConst.unsafeFromPtr pStr) pOut
      val <- peek pOut
      pure (fromIntegral val, fromIntegral status)
```

What we want them to write:

```haskell
hsParseInt :: String -> IO (Int, Int)
hsParseInt s = do
  (cval, status) <- withOut $ \pOut -> hl parse_int s pOut
  pure (fromIntegral cval, fromIntegral status)
```

### 2.4 A real example — `srp6_server_session_step1`

The most complex example in `examples/botan/hs-project/src/Botan.hs`
(lines 109-136). The C function:

```c
int botan_srp6_server_session_step1(
    botan_srp6_server_session_t   session,
    const uint8_t *               verifier, size_t verifier_len,
    const char *                  group_id,
    const char *                  hash_id,
    botan_rng_t                   rng,
    uint8_t *                     B,        size_t *B_len);
```

The low-level binding `hs-bindgen` generates:

```haskell
botan_srp6_server_session_step1
  :: Botan_srp6_server_session_t
  -> PtrConst Word8 -> CSize
  -> PtrConst CChar
  -> PtrConst CChar
  -> Botan_rng_t
  -> Ptr Word8 -> Ptr CSize
  -> IO CInt
```

What the user writes today: **18 lines** of `withCString` / `allocaBytes`
/ `alloca` / `poke` / `peek` / `fromIntegral` / `peekArray`:

```haskell
srp6ServerSessionStep1 :: ServerSession -> Verifier -> GroupId -> HashId -> RNG -> IO B
srp6ServerSessionStep1 (ServerSession s) (Verifier verifier) groupId hashId
                       (RNG rngObj) =
    IsA.withElemPtr verifier $ \verifierPtr ->
    let verifierLen = fromIntegral $ VS.length $ IA.toVector verifier in
    withCString (groupIdString groupId) $ \groupIdPtr ->
    withCString (hashIdString hashId)   $ \hashIdPtr ->
    srp6GroupSize groupId >>= \maxLen ->
    allocaBytes (fromIntegral maxLen) $ \bPtr ->
    alloca $ \bLenPtr -> do
      poke bLenPtr maxLen
      _ <- botan_srp6_server_session_step1 s
             (PtrConst.unsafeFromPtr verifierPtr) verifierLen
             (PtrConst.unsafeFromPtr groupIdPtr)
             (PtrConst.unsafeFromPtr hashIdPtr)
             rngObj bPtr bLenPtr
      bLen <- peek bLenPtr
      B <$> IA.peekArray (fromIntegral bLen) (IA.toPtr bPtr)
```

What we want them to write:

```haskell
srp6ServerSessionStep1
  :: ServerSession -> Verifier -> GroupId -> HashId -> RNG -> IO B
srp6ServerSessionStep1 ses ver gid hid rng = do
  maxLen <- srp6GroupSize gid
  fmap (B . fst) $ withBuf (fromIntegral maxLen) $ \pBuf pLen ->
    hl botan_srp6_server_session_step1 ses ver gid hid rng pBuf pLen
```

The body fetches the capacity hint the same way the hand-written version does
(`srp6GroupSize groupId`), then composes `withBuf` (which handles the
buffer-plus-length output convention) with `hl` (which handles the boring
conversions for the input arguments).

<a id="3-state-of-the-art"></a>

## 3. State of the art

### 3.1 c2hs — the existing Haskell tool

c2hs is a preprocessor that reads C headers and rewrites `{# fun #}` /
`{# enum #}` / `{# pointer #}` / `{# get #}` / `{# set #}` directives in
a `.chs` file into a Haskell module with `foreign import` declarations
and matching marshalling wrappers.

Two small examples:

```c
int do_thing(const char *str, int len);
int get_size(size_t *out);
```

In c2hs:

```haskell
{#fun do_thing as ^
   { withCStringLen* `String'& } -> `Int' #}

{#fun get_size as ^
   { alloca- `Int' peekIntConv* } -> `Int' #}
```

The first declaration says: take one Haskell `String` (the back-ticked
`String'`), use `withCStringLen` as its input marshaller (the `*` after a
function name means "an `IO` bracket"), and splat the resulting
`(Ptr CChar, Int)` across two C arguments (the trailing `&` on the
type). Generated Haskell:

```haskell
doThing :: String -> IO Int
doThing s = withCStringLen s $ \(p, n) ->
              fromIntegral <$> do_thing'_ p (fromIntegral n)
```

The second says: this argument is suppressed from the Haskell signature
(`alloca-`, the trailing `-` on the marshaller, means don't expose it), the
marshaller allocates a `size_t*`, and after the call we `peekIntConv*` it into
an `Int` (the trailing `*` again being IO, appended to the result tuple).
Generated:

```haskell
getSize :: IO Int
getSize = alloca $ \p -> get_size'_ p >> fromIntegral <$> peek p
```

The full grammar adds ~6 modifiers: `*` (IO bracket), `-` (suppress
from signature), `&` (one-Hs-to-two-C), `*-` (effect-only, like a
discard-result error check), `+`/`+S`/`+N` (allocate scratch and append
to result), `%` (struct by value via a generated C wrapper). c2hs picks
a sensible default marshaller per `(Hs type, C type)` pair, so users
typically write just `` `String' `` and get `withCString*` /
`peekCString*` for free.

**What c2hs gets right.** The decomposition of an argument position into *(in
marshaller, out marshaller, modifiers)* seems to be the right conceptual unit.
Every binding boils down to this shape. The `&` modifier serves a real, common
pattern (`1 Hs → N C slots`) and solves it for `N=2`.

However, c2hs has limits: it is a preprocessor on top of Haskell rather than
a Haskell library, the `&` modifier is hard-coded to N=2 (a struct mapped to
three C slots is not expressible), and the generated code can be hard to read.

<a id="4-what-hs-bindgen-ships-today"></a>

## 4. What `hs-bindgen` ships today

Three pieces of the existing runtime inform the design.

**`HasFFIType`** strips newtype wrappers off Haskell types so `foreign import`
declarations can be expressed in the underlying primitive C types
(`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/HasFFIType.hs:103-114`).
Pure and total, trivial instances cover the primitive C types and the
project's newtype wrappers. It does not support `IO` brackets, so it isn't
directly what we need at the high-level layer. But it is the shape we're after
for the no-bracket subset.

**`ToFunPtr` / `FromFunPtr` / `withFunPtr`** handle bidirectional
marshalling for callbacks
(`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/FunPtr/Class.hs:23-41`):

```haskell
class ToFunPtr a where
  toFunPtr :: a -> IO (FunPtr a)

class FromFunPtr a where
  fromFunPtr :: FunPtr a -> a

withFunPtr :: ToFunPtr a => a -> (FunPtr a -> IO b) -> IO b
withFunPtr x = bracket (toFunPtr x) freeHaskellFunPtr
```

A pair of classes, one per direction, plus a `bracket`-derived helper that
uses both. This is the shape we want for general marshalling, however it only
exists for function pointers.

The remaining support types are already in place and we'll use them as-is:
`PtrConst`, `IncompleteArray`, `IsArray` (with `withElemPtr`), `HasCField`
for struct access, `CEnum` for enums, `StaticSize` / `ReadRaw` / `WriteRaw`
plus `EquivStorable` for low-level memory marshalling.

### 4.1 What is missing

`HasFFIType` is pure; `ToFunPtr`/`FromFunPtr` is bidirectional and
effectful but exists only for function pointers. The gap is:

- A general conversion class for arguments, same shape as
  `ToFunPtr` but for any Haskell type, with `IO` brackets for allocation.
- A way to express that one Haskell value occupies *multiple* C slots
  (the c2hs `&` case): `ByteString` covers `(PtrConst CChar, CSize)`, a
  `Vector` covers `(Ptr a, CSize)`, etc.
- A glue combinator that walks a user's high-level signature and a
  low-level binding's signature together, picking the right conversion
  per slot.

<a id="5-servants-type-class-elaboration-mechanism"></a>

## 5. Servant's type-class elaboration mechanism

The design relies on a type-class technique that is used in `servant`. A brief
tour first makes §6 less mysterious.

In `servant`, an API is described as a *type*:

```haskell
type UsersAPI =
       "users" :> Capture "id" Int :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] User
```

The user provides a *handler* whose shape matches the API:

```haskell
handler :: Server UsersAPI
handler = getUser :<|> createUser
  where
    getUser    :: Int     -> Handler User
    createUser :: NewUser -> Handler User
```

The handler's shape (`Int -> Handler User`, `NewUser -> Handler User`)
is computed from the API type by a type-family expansion. It's driven
by the `HasServer` class, which has one instance per combinator
(`:>`, `Capture`, `ReqBody`, `Get`, `:<|>`, …). The compiler walks the
API type left-to-right, and each combinator's instance contributes
either an argument to the handler (`Capture "id" Int` adds an `Int`
argument) or a piece of the response (`Get '[JSON] User` says the
handler returns `Handler User`).

The technique has a known cost: error messages. When the user makes a small
mistake (wrong type at a captured slot, wrong arity, missing JSON instance),
GHC's default error message is a multi-screen wall of failed type-family
reductions. Servant has some ways to mitigate this and emit focused
diagnostics.

<a id="6-design"></a>

## 6. Design

Three pieces, motivated by the §2 examples:

1. Two classes describing per-type conversion: `ToC` for arguments,
   `FromC` for results, plus a small `Nullable` helper that lifts to
   `ToC (Maybe a)` (§6.1).
2. The `hl` combinator that walks a high-level and a low-level signature
   in lockstep, applying `ToC`/`FromC` (§6.2). One recursive `HighLevel` instance
   plus two small helper classes (`Spread`, `Thread`) handle arbitrary
   arity and arbitrary slot shapes.
3. A handful of result-side combinators (`withOut`, `withBuf`) for
   patterns that cannot be inferred from types (§6.3).

### 6.1 `ToC` and `FromC`

The starting point: generalise the `ToFunPtr`/`FromFunPtr` shape from
callbacks to any Haskell type, with `IO` brackets so that
`withCString`-style allocation is expressible.

```haskell
class ToC a where
  type CSlot a :: Type
  type CSlot a = a -- default
  withC :: a -> (CSlot a -> IO r) -> IO r
  default withC :: CSlot a ~ a => a -> (CSlot a -> IO r) -> IO r
  withC x k = k x

class FromC c hs where
  fromC :: c -> hs
```

`CSlot a` is the *type* of the C slot that this Haskell value occupies.
It resolves to a single type (using a tuple for multi-slot values) rather
than a curried chain of arrows, which keeps GHC's type-family reduction predictable. For Haskell values
that occupy multiple consecutive C arguments, the slot is a *tuple*: `CSlot
ByteString = (PtrConst CChar, CSize)`. The `Spread` helper in §6.2 applies the
tuple to the curried C function automatically; users never write the tuple
application by hand.

`withC` has `IO`-bracket semantics: the value passed to the continuation
is valid for the duration of the continuation, and any scratch memory is
released afterward.

The defaults (`type CSlot a = a` and `withC x k = k x`) make the identity
instance one line:

```haskell
instance ToC CChar
instance ToC CInt
instance ToC CSize
-- and so on, for the entire Foreign.C.Types zoo
```

Override `CSlot` and `withC` together for real conversions:

```haskell
instance ToC String where
  type CSlot String = PtrConst CChar
  withC s k = withCString s (k . PtrConst.unsafeFromPtr)

instance ToC ByteString where
  type CSlot ByteString = (PtrConst CChar, CSize)
  withC bs k = BS.useAsCStringLen bs $ \(p, n) ->
                 k (PtrConst.unsafeFromPtr p, fromIntegral n)
```

`FromC` is the dual but **pure**: the C side of a return value is fixed by the
FFI signature, so there is no need for an associated type or for `IO`:

```haskell
instance Integral cn => FromC cn Int     where fromC = fromIntegral
instance Integral cn => FromC cn Word    where fromC = fromIntegral
instance Integral cn => FromC cn Integer where fromC = fromIntegral

instance FromC CBool   Bool   where fromC = Marshal.toBool
instance FromC CFloat  Float  where fromC = realToFrac
instance FromC CDouble Double where fromC = realToFrac

instance {-# OVERLAPPABLE #-} FromC c c where fromC = id
```

#### 6.1.1 Width-bearing integers

There is intentionally **no** `ToC Int` instance. C has many integer widths
(`CInt`, `CSize`, `CLong`, `CLLong`, …) and a bare `Int` on the Haskell side
does not say which one is intended. The design forces the user to be explicit.
Two idioms:

```haskell
-- Idiom 1: write the C width directly on the Haskell side.
hsRead :: CInt -> Ptr CChar -> CSize -> IO Int
hsRead = hl c_read

-- Idiom 2: a meaning-carrying newtype that picks the width. Note that
-- the two newtypes have different withC bodies: Fd wraps the C value
-- directly (just unwrap), Bytes wraps an Int (convert at boundary).
newtype Fd    = Fd    CInt
newtype Bytes = Bytes Int

instance ToC Fd where
  type CSlot Fd = CInt
  withC (Fd n)    k = k n -- no conversion

instance ToC Bytes where
  type CSlot Bytes = CSize
  withC (Bytes n) k = k (fromIntegral n) -- widen at boundary

hsRead' :: Fd -> Ptr CChar -> Bytes -> IO Int
hsRead' = hl c_read
```

Idiom 2 is the **recommended** pattern for any `Int`-shaped value that
carries a domain meaning. The newtype name documents the role, the `CSlot`
instance documents the C width, and the call site reads honestly.

`ToC Bool` is the one exception to the "don't hardwire" rule: C has exactly
one boolean type (`CBool`), so the mapping is forced and unambiguous. However,
the user is still free to define their own custom Haskell `Bool` type and map it
to `CBool`.

#### 6.1.2 Multi-slot via tupled `CSlot`

For Haskell values that occupy multiple consecutive C arguments (the c2hs
`&` case), the slot is a tuple. We saw `ByteString` above. `IncompleteArray`
and its const-pointer companion follow the same shape:

```haskell
-- Mutable array slot, for in/out parameters.
instance Storable a => ToC (IncompleteArray a) where
  type CSlot (IncompleteArray a) = (Ptr a, CSize)
  withC arr k = IsA.withElemPtr arr $ \p ->
    k (p, fromIntegral (VS.length (IA.toVector arr)))

-- Const-pointer slot, for input-only @const T *@ parameters.
newtype ConstIncompleteArray a = ConstIncompleteArray (IncompleteArray a)

instance Storable a => ToC (ConstIncompleteArray a) where
  type CSlot (ConstIncompleteArray a) = (PtrConst a, CSize)
  withC (ConstIncompleteArray arr) k = IsA.withElemPtr arr $ \p ->
    k (PtrConst.unsafeFromPtr p, fromIntegral (VS.length (IA.toVector arr)))
```

A user who has a domain newtype around `IncompleteArray Word8` (say,
`Verifier`) picks the const or mutable shape via the wrapping:

```haskell
newtype Verifier = Verifier (ConstIncompleteArray Word8)
  deriving newtype ToC
```

The same shape extends past 2 slots. A `Matrix` value naturally packs
into three: a data pointer, a row count, and a column count. Against

```c
int matrix_trace(const int *data, int rows, int cols, int *out);
```

the user defines

```haskell
data Matrix a = Matrix
  { rows  :: !Int
  , cols  :: !Int
  , elems :: !(VS.Vector a)
  }

instance ToC (Matrix a) where
  type CSlot (Matrix a) = (PtrConst a, CInt, CInt)
  withC = ...  -- bracket the element buffer, hand (ptr, rows, cols) to k
```

and writes the wrapper:

```haskell
hsMatrixTrace :: Matrix CInt -> IO (CInt, Int)
hsMatrixTrace m = do
  (trace, status) <- withOut $ \pOut -> hl matrix_trace m pOut
  pure (trace, fromIntegral status)
```

The 3-tuple `Spread` instance fires; nothing else changes. The same
shape generalises to N dimensions by widening the slot tuple (or, for
variable rank, by encoding the shape as a separate C array).

#### 6.1.3 Nullability

`Maybe a` marshals as a nullable pointer: `Nothing` is the slot's null,
`Just x` delegates to the wrapped instance. A tiny helper class
`Nullable` carries the "this slot has a null" capability:

```haskell
class Nullable a where
  nullValue :: a

instance Nullable (Ptr a)      where nullValue = nullPtr
instance Nullable (PtrConst a) where nullValue = PtrConst.unsafeFromPtr nullPtr

instance (ToC a, Nullable (CSlot a)) => ToC (Maybe a) where
  type CSlot (Maybe a) = CSlot a
  withC Nothing  k = k nullValue
  withC (Just x) k = withC x k
```

The `Nullable (CSlot a)` constraint rules out non-pointer slots and multi-slot
tuples at instance resolution. `FunPtr` could get its own `Nullable` instance
the same way.

### 6.2 The `hl` combinator

`ToC` and `FromC` describe how individual Haskell types relate to C slots.
We now need a way to *apply* those conversions across a whole function
signature. This is the elaboration trick from `servant`, applied to
function arrows.

The combinator the user calls:

```haskell
hl :: HighLevel hi lo => lo -> hi
```

`HighLevel hi lo` walks the user's high-level signature `hi` and the generated
low-level signature `lo` in lockstep. There are two instances: the result-step
base case and the argument-step recursive case.

```haskell
class HighLevel hi lo where
  hl :: lo -> hi

-- Base case: convert the C-side return value.
instance FromC c hs => HighLevel (IO hs) (IO c) where
  hl mc = fromC <$> mc

-- Recursive case: any Hs arity, any CSlot shape.
instance
     ( ToC a
     , Spread (CSlot a) lo loSpread
     , Thread (CSlot a) loSpread
     , HighLevel rest loSpread
     )
  => HighLevel (a -> rest) lo where
    hl loFn = \x ->
      hl @rest (thread (withC x) (\slot -> spread slot loFn))
```

Two helper classes are used inside the recursive instance: `Spread` handles
tupled slots; `Thread` threads the bracket through the remaining function
shape.

#### 6.2.1 `Spread`: applying a tupled slot

When `CSlot a` is a tuple (the c2hs `&` case), the slot has to be applied
to the *curried* C function. `Spread` does that, with one instance per
tuple arity:

```haskell
class Spread t lo lo' | t lo -> lo' where
  spread :: t -> lo -> lo'

instance Spread (a, b)       (a -> b -> lo)            lo where spread (a, b)       f = f a b
instance Spread (a, b, c)    (a -> b -> c -> lo)       lo where spread (a, b, c)    f = f a b c
instance Spread (a, b, c, d) (a -> b -> c -> d -> lo)  lo where spread (a, b, c, d) f = f a b c d
instance {-# OVERLAPPABLE #-} Spread t (t -> lo) lo where spread t f = f t -- non-tuple fallback
```

Tuple arities in real C functions are small and bounded; we offer up to
4-tuples. The `OVERLAPPABLE` non-tuple fallback handles the common `CSlot a =
a` case (a single slot, no tuple).

#### 6.2.2 `Thread`: structural recursion on the function arrow

`Thread` threads a `with`/bracket function through whatever shape is left of the
low-level function after the slot has been spread:

```haskell
class Thread b g where
  thread :: (forall r. (b -> IO r) -> IO r) -> (b -> g) -> g

instance Thread b (IO r) where -- base
  thread br f = br f

instance Thread b rest => Thread b (arg -> rest) where -- step
  thread br f = \arg -> thread br (\b -> f b arg)
```

Concretely, lifting `c_strncmp :: PtrConst CChar -> PtrConst CChar -> CSize ->
IO CInt` through one `String` argument first calls

```
thread (withC s) (\slot -> spread slot c_strncmp) :: PtrConst CChar -> CSize -> IO CInt
```

`Thread` recurses through the remaining two arguments, introducing one
lambda each:

```
thread … = \arg1 -> thread … (\b -> c_strncmp b arg1)                  -- step
         = \arg1 -> \arg2 -> thread … (\b -> c_strncmp b arg1 arg2)    -- step
         = \arg1 -> \arg2 -> withC s (\b -> c_strncmp b arg1 arg2)     -- base
```

The bracket from `withC s` fires at the innermost `IO`, after both
remaining arguments have been received.

#### 6.2.3 Walking through `hl strlen`

Given:

```haskell
strlen   :: PtrConst CChar -> IO CSize
hsStrlen :: String         -> IO Int
hsStrlen = hl strlen
```

GHC elaborates as follows:

1. `hi = String -> IO Int`. The recursive `HighLevel` instance fires.
   `ToC String` says `CSlot String = PtrConst CChar` (single slot, no
   tuple). `Spread` picks the non-tuple fallback: the slot is consumed
   by the first arg of `lo`. `Thread` threads `withCString`'s bracket
   through the remaining function shape (here just `IO CSize`, the base
   case fires).
2. The recursive goal is `HighLevel (IO Int) (IO CSize)`. The base
   `HighLevel` instance fires. `FromC CSize Int` is satisfied by
   `Integral CSize => FromC CSize Int`. Done.

#### 6.2.4 Walking through `hl c_strncmp` (mixed slots)

Given:

```haskell
c_strncmp      :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt
hsStrncmpMixed :: String -> ByteString             -> IO Int
hsStrncmpMixed = hl c_strncmp
```

The recursive `HighLevel` fires twice:

1. **`String`** (one slot). `Spread` non-tuple fallback consumes one C arg.
   `Thread` step case threads the `withCString` bracket through the
   remaining two args.
2. **`ByteString`** (two slots, tuple). `Spread` 2-tuple instance consumes
   both `PtrConst CChar` and `CSize`. `Thread` base case fires (the remaining
   shape is `IO CInt`); the `useAsCStringLen` bracket fires here.

Then the base `HighLevel` instance converts `IO CInt` to `IO Int` via
`FromC CInt Int`. One recursive elaboration handles different slot
arities in the same signature.

### 6.3 Out parameters and other result-side cases

Some C functions don't return their interesting value directly: they
write it into a caller-allocated pointer. From §2.3:

```c
int parse_int(const char *s, int *out);
```

The signature the user *wants* is something like `String -> IO Int`,
but the library can't infer which `Int`-shaped path to expose. The
low-level signature `PtrConst CChar -> Ptr CInt -> IO CInt` has two
`Int`-shaped candidates: the `CInt` return value (a status code) and
the `CInt` written into the out parameter (the parsed value).

The right answer is to *make the user pick* with a small combinator. Two
combinators for now:

```haskell
-- "There is one out parameter, of C type c, and the C return value is
-- delivered alongside the peeked value."
--
-- The return tuple is (out-param-value, foreign-return-value).
withOut :: Storable c => (Ptr c -> IO r) -> IO (c, r)
withOut k = alloca $ \p -> do
              r <- k p
              c <- peek p
              pure (c, r)

-- "There is a buffer of capacity n, and a length pointer that's pre-poked
-- with n and read back after the call."
--
-- Mirrors withOut: the return tuple is (buffer-contents, foreign-return-value).
withBuf :: forall a r. Storable a
        => Int -- ^ capacity in elements
        -> (Ptr a -> Ptr CSize -> IO r)
        -> IO (IncompleteArray a, r)
withBuf cap k = do
  let bytes = cap * sizeOf @a undefined
  allocaBytes bytes $ \pBuf ->
    alloca $ \pLen -> do
      poke pLen (fromIntegral cap)
      r <- k pBuf pLen
      n <- peek pLen
      arr <- IA.peekArray (fromIntegral n) (IA.toPtr pBuf)
      pure (arr, r)
```

The user composes (the §2.3 wrapper, repeated here for context):

```haskell
hsParseInt :: String -> IO (Int, Int)
hsParseInt s = do
  (cval, status) <- withOut $ \pOut -> hl parse_int s pOut
  pure (fromIntegral cval, fromIntegral status)
```

For the full §2.4 example:

```haskell
srp6ServerSessionStep1
  :: ServerSession -> Verifier -> GroupId -> HashId -> RNG -> IO B
srp6ServerSessionStep1 ses ver gid hid rng = do
  maxLen <- srp6GroupSize gid
  fmap (B . fst) $ withBuf (fromIntegral maxLen) $ \pBuf pLen ->
    hl botan_srp6_server_session_step1
       ses ver gid hid rng pBuf pLen
```

The user's signature is exactly the domain signature; the body is a small
composition. `hl` handles the in-parameter conversions (`ses ver gid hid rng`),
including the `Verifier` newtype around `ConstIncompleteArray Word8`. The
`withBuf` handles the buffer-plus-length output convention explicitly because
that convention cannot be inferred from the result type alone.
