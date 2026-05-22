# High-level bindings for `hs-bindgen` — design

> Canonical design for [hs-bindgen issue #8][issue-8]. Companion document:
> [`ISSUE_ANALYSIS.md`](./ISSUE_ANALYSIS.md), which contains the workflow audit
> trail (plans considered, evaluator review, blast radius, citation
> verification).
>
> **Status**: implemented under `hs-bindgen-runtime/src/HsBindgen/Runtime/HighLevel/`,
> with smoke tests in `hs-bindgen-runtime/test/Test/HsBindgen/Runtime/HighLevel/Smoke.hs`
> exercising the design against `libc` (`strlen`, `memcmp`, `strncmp`),
> `withOut`/`withBuf`, mixed-slot signatures, nullable `Maybe`, and
> arbitrary arity.
>
> **Audience**: `hs-bindgen` contributors and the Well-Typed team. Assumes
> familiarity with the codebase and Haskell idioms (deriving-via,
> type families, GADT-flavoured `IO` continuations).

[issue-8]: https://github.com/well-typed/hs-bindgen/issues/8

---

## Table of contents

1. [Problem and context](#1-problem-and-context)
2. [Worked example](#2-worked-example)
3. [State of the art](#3-state-of-the-art)
4. [Where `hs-bindgen` is today](#4-where-hs-bindgen-is-today)
5. [The type-class elaboration trick (a brief tour of servant)](#5-the-type-class-elaboration-trick-a-brief-tour-of-servant)
6. [Design](#6-design)
7. [Inspecting and testing the design](#7-inspecting-and-testing-the-design)
8. [Caveats](#8-caveats)

---

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

## 2. Some C example patterns

Three C functions, in increasing complexity. For each: the C signature, the
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

The conversions that need to happen:

- `String ↔ Ptr CChar` (with `withCString`-bracket semantics);
- `CSize ↔ Int` (with `fromIntegral`);

Are obvious from the types alone. A library should be able to discover them
and apply them.

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
// Parse a string as a 32-bit integer. Writes the parsed value into
// *out; returns 0 on success, -1 on parse error.
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

Two combinators, three lines. `hl` walks the curried low-level
signature: it brackets `s :: String` into a `PtrConst CChar` via
`withCString`, passes the `pOut :: Ptr CInt` through unchanged, and
applies the result. `withOut` allocates the out slot and peeks it
after the call. The user picks where the pointer goes — `pOut` is
just inserted at its position in the curried call.

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

However, the DSL is a preprocessor on top of Haskell; the `&` modifier is
hard-coded to N=2; arbitrary N (a struct mapped to three C slots) is not
expressible; the generated code can be hard to read.

## 4. What `hs-bindgen` ships today

Three pieces of the existing runtime inform the design.

**`HasFFIType`** strips newtype wrappers off Haskell types so `foreign import`
declarations can be expressed in the underlying primitive C types
(`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/HasFFIType.hs:103-114`).
Pure and total, trivial instances cover the primitive C types and the
project's newtype wrappers. It does not support `IO` brackets so it isn't
directly what we need at the high-level layer; but it is the shape we're after
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

## 5. Servant's type-class elaboration mechanism

The design relies on a type-class technique that is used in `servant`. Showing
it briefly here lets the design that follows look less mysterious.

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
It is a type with a single type variable and not a curried chain of arrows
because that keeps GHC's type-family reduction predictable. For Haskell values
that occupy multiple consecutive C arguments, the slot is a *tuple*: `CSlot
ByteString = (PtrConst CChar, CSize)`. The `Spread` helper in §6.2 applies the
tuple to the curried C function automatically; users never write the tuple
application by hand.

`withC` uses `IO`-bracket semantics: the value passed to the continuation
is valid for the duration of the continuation, and any scratch memory is
released afterward.

The defaults: `type CSlot a = a` and `withC x k = k x`, make the identity
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
  withC (Fd n)    k = k n                    -- no conversion

instance ToC Bytes where
  type CSlot Bytes = CSize
  withC (Bytes n) k = k (fromIntegral n)     -- widen at boundary

hsRead' :: Fd -> Ptr CChar -> Bytes -> IO Int
hsRead' = hl c_read
```

Idiom 2 is the **recommended** pattern for any `Int`-shaped value that
carries a domain meaning. The newtype name documents the role, the `CSlot`
instance documents the C width, and the call site reads honestly.

`ToC Bool` is the one exception to the "don't hardwire" rule: C has exactly
one boolean type (`CBool`), so the mapping is forced and unambiguous.

The asymmetry between `ToC` and `FromC` here is deliberate. On the argument
side the Haskell type is the user's choice and the C side is fixed by the
FFI signature, so the user has to pick a width. On the result side the C
type is fixed and the Haskell type is the user's choice; `FromC`'s open
class lets `fromIntegral` cover the whole zoo automatically.

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

newtype ConstMatrix a = ConstMatrix (Matrix a)

instance ToC (ConstMatrix a) where
  type CSlot (ConstMatrix a) = (PtrConst a, CInt, CInt)
  withC = ...  -- bracket the element buffer, hand (ptr, rows, cols) to k
```

and writes the wrapper:

```haskell
hsMatrixTrace :: ConstMatrix CInt -> IO (CInt, Int)
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

The `Nullable (CSlot a)` constraint rules out non-pointer slots and
multi-slot tuples at instance resolution: `ToC (Maybe Int)` and
`ToC (Maybe ByteString)` are both compile errors, deliberately — there
is no obvious null for an integer or a `(ptr, len)` pair. `FunPtr`
could get its own `Nullable` instance the same way once a callback
story lands.

### 6.2 The `hl` combinator

`ToC` and `FromC` describe how individual Haskell types relate to C slots.
We now need a way to *apply* those conversions across a whole function
signature. This is the elaboration trick from `servant`, applied to
function arrows.

The combinator the user calls:

```haskell
hl :: HighLevel hi lo => lo -> hi
```

`HighLevel hi lo` walks the user's high-level signature `hi` and the
generated low-level signature `lo` in lockstep. There are exactly **two**
instances: the result-step base case and the argument-step recursive case.
Arbitrary Hs arity is supported because the recursive instance recurses
structurally on `(->)`.

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
instance {-# OVERLAPPABLE #-} Spread t (t -> lo) lo where spread t f = f t  -- non-tuple fallback
```

Tuple arities in real C functions are small and bounded, we offer up to
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

The right answer is to *make the user pick* with a small combinator. We offer
two, for now:

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

### 6.4 Module organisation

```
HsBindgen.Runtime.HighLevel.ToC      -- ToC class, CSlot type family, ConstIncompleteArray
HsBindgen.Runtime.HighLevel.FromC    -- FromC class
HsBindgen.Runtime.HighLevel.Call     -- HighLevel class (method: hl), Spread, Thread
HsBindgen.Runtime.HighLevel.Result   -- withOut, withBuf
HsBindgen.Runtime.HighLevel.Prelude  -- convenience re-exports
```

`Spread` and `Thread` are exported from `Call` for advanced users but **not**
re-exported from `Prelude` — they are elaboration mechanism, not user-facing
API. Users get them transparently via `hl`.

Convention matches the existing `HsBindgen.Runtime.*` shape. Users
import qualified:

```haskell
import HsBindgen.Runtime.HighLevel.Prelude qualified as HL

hsStrlen :: String -> IO Int
hsStrlen = HL.hl strlen
```

### 6.5 Use from the PP and TH backends

The classes are ordinary Haskell — they can be referenced from
declarations in both the Preprocess backend (which writes `.hs` files
at build time) and the Template Haskell backend (which produces
declarations via splices). No special build dance.

For now, *neither* backend emits `hl` calls — Milestone 2.5 is
library-only, so the user manually writes the `hl` line. Milestone 3
generation is the natural next step and is supported by the design.

## 7. Inspecting and testing the design

A reasonable concern about type-class-and-type-family-heavy code is:
"how do I know what it does?" Two answers.

### 7.1 Inspecting the elaborated code

`hl strlen` is not a code-generator in any real sense: there is no
splice, no preprocessing, no emitted source file. GHC just resolves
type-class instances and inlines them. The "code" the design produces
is just the chain of inlined `withC` / `fromC` / `spread` / `thread`
bodies.

To see what GHC actually does, use:

```bash
ghc -ddump-simpl -dsuppress-all -ddump-to-file foo.hs
```

This dumps the optimised Core (the program after the simplifier runs).
For a properly `INLINE`d `hl strlen`, the dumped Core should be
identical (modulo names) to the hand-written wrapper. If it is not,
either an `INLINE` pragma is missing or a `Storable` instance is
non-trivial; both are fixable.

For a quick check that the elaboration *type-checks* as expected
without committing to Core inspection, GHC's `:type` in GHCi suffices:

```
ghci> :type hl strlen
hl strlen :: String -> IO Int
```

### 7.2 Testing strategy

Three layers:

1. **Smoke tests against libc.**
   `hs-bindgen-runtime/test/Test/HsBindgen/Runtime/HighLevel/Smoke.hs`
   exercises `hl` against `strlen`, `memcmp`, `strncmp` (the mixed-slot
   case: `String + ByteString` in one signature), `withOut`,
   `withBuf`, and an arbitrary-arity (5-arg) wrapper. Build + test
   verifies the full elaboration end-to-end against a real linker.
2. **Property tests** for round-trip semantics on each `ToC`/`FromC`
   instance pair (`fromC . withC ≡ pure`-ish). The runtime test suite
   already uses Hedgehog/QuickCheck and can host these.
3. **Integration tests in `examples/`.** Port `c-qrcode/Main.hs` and
   `botan/src/Botan.hs` to use the new API. Real C, real lifetimes;
   if they break, something real is wrong.

Inspection-style tests with frozen Core (`inspection-testing`) are
worth adding later if performance regressions show up.

[inspection-testing]: https://hackage.haskell.org/package/inspection-testing

---

## 8. Caveats

Where the design might not work as well as we want, in priority order.

### 8.1 The `HighLevel` class will produce bad error messages by default

The risk that §5 set up. The elaboration is morally identical to
`servant`'s — a type class that recurses on `(->)` with help from type
families and helper classes — and the same error-message rot applies.
When the user misspells a type, omits an instance, or has an arity
mismatch, GHC's default error message will be a wall of failed
`Spread`/`Thread`/`HighLevel` reductions.

**Mitigation**, kept open but worth recording:

- Add `TypeError`-clad instance heads that detect the common failure
  modes (missing `ToC Foo` → "Add a `ToC Foo` instance or use a domain
  newtype"; arity mismatch → "Expected N arguments, got M").
- Add golden-text tests for the error messages so that future
  refactors don't regress them.

If the polish is skimped, the design will earn servant's
hard-error-message reputation unfairly. We need budget for it in the
implementation phase.

### 8.2 Some result-side conventions need explicit combinators

By construction (§6.3) we cannot infer "this C function returns its
result through an out parameter" from types alone. The user has to
opt in by writing `withOut`, `withBuf`, etc. That is a deliberate
choice: the design favours legibility ("what does this wrapper do?
read its body") over magic.

The risk is that the set of combinators grows: `withOut`, `withBuf`,
`withInOut` (capacity-cap pattern), `withOut2` (two out parameters
returned as a tuple), `withOptionalOut`. If the catalog expands beyond
a handful we should revisit and consider folding them into a single
class with deriving-via newtypes — coming back to a wrapper after all,
just internalised in the combinator's body rather than exposed in user
signatures.

### 8.3 Tupled `CSlot` + `Spread` + `Thread`: the design choice

An earlier draft of this document worried that an N-ary curried
type family `AsC a r = c1 -> c2 -> … -> r` might not survive contact
with GHC's type-family reducer, because the recursion has to reduce
partial applications. That worry was justified — the curried form had
problems at instance heads with `~`-equality constraints, and GHC
didn't reduce when we wanted.

The shipped design avoids the issue: `CSlot a` is one type, possibly
a tuple, and the application across a curried low-level function is
handled at the value level by `Spread`. `Thread` then handles the
structural recursion on the function arrow. The result is two `HighLevel`
instances total (base + recursive), each non-overlapping, plus three
small helper instances per tuple arity.

The cost is that `Spread` needs an instance per tuple arity. We ship
up to 4-tuples; real C functions don't have a single Haskell value
contributing more than 2–3 slots. If a 5-slot case ever appears,
adding an instance is mechanical.

### 8.4 Performance pessimisations are possible

The bracket-style `withC :: a -> (... -> IO r) -> IO r` composes
cleanly but adds a closure allocation per argument unless GHC inlines
aggressively. We ship `INLINE` pragmas on `withC` / `fromC` /
`spread` / `thread` and on the recursive `HighLevel` body for that reason.
Verifying via Core dump (see §7.1) is the cheap check.

The performance baseline is the hand-written wrapper, which has the
same shape. We should not be slower than that. If we are, an `INLINE`
pragma is missing or a `Storable` instance is non-trivial.

### 8.5 `FromC cn Int` for all integer C types is a strong default

§6.1 adopts the ergonomic instance — `Integral cn => FromC cn Int`
via `fromIntegral`. This is what c2hs's `cIntConv` does and is the
convenience users overwhelmingly want: write `Int` regardless of
whether the C side is `CInt`, `CSize`, `CLong`, etc. The downside is
that narrowing conversions silently lose information (`CLLong → Int`
on a 32-bit platform; `Word32 → Int` on a 32-bit platform near the
top of the range).

The mitigation is documentation, not redesign: the haddock for
`FromC` flags the overflow risk loudly and recommends users write
explicit `Int32` / `Int64` / `Word` (or use idiom 2 from §6.1.1)
when the C side has a known exact width that matters.

### 8.6 We deliberately exclude exception semantics from the default

A C function returning `-1 on error` is the most common convention in
C APIs. The proposal does not bake any exception-throwing into the
defaults; users layer their own error-handling in. The risk is that
users will reinvent `throwErrnoIfNegative` everywhere by hand. The
mitigation is to ship a handful of small `IO`-shaped wrappers
(`throwErrnoIfNegative`, `throwErrnoIfNull`, etc., probably already
in `base`) and document the convention in the manual.

---

## Appendix A: open questions tracker

The questions raised in [`ISSUE_ANALYSIS.md`](./ISSUE_ANALYSIS.md) §7,
with their final status:

| Q | Topic | Resolution |
|---|-------|------------|
| Q1 | Nullability — separate class or in main class? | Folded into `ToC` via constraint on the `Maybe` instance, with a small `Nullable` helper class carrying the "this slot has a null" capability. §6.1.3. |
| Q2 | Default error-handling | Explicit non-feature; users layer their own. §8.6. |
| Q3 | `Storable` vs `ReadRaw`/`WriteRaw` | Non-issue in practice; `EquivStorable` bridges. Any type used by `withOut`/`withBuf` should derive `Storable` via `EquivStorable`. |
| Q4 | Naming | `ToC` / `FromC` (mirrors `ToFunPtr` / `FromFunPtr`). Multi-slot via tupled `CSlot`, no wrapper newtypes in user signatures. Result-side combinators are `withOut` / `withBuf`. §6. |
| Q5 | `HList` vs N-ary curried continuation vs tupled slot | **Tupled `CSlot` + `Spread` + `Thread`**. Pure curried (`c1 -> c2 -> r` as a type family) hit instance-head reduction issues; pure `HList` was heavy on machinery. Tupled slots compromise: one type per `ToC` instance, value-level spread, structural arrow recursion in a separate class. §6.1, §6.2, §8.3. |
| Q6 | Module namespace | `HsBindgen.Runtime.HighLevel.*`. §6.4. |
| Q7 | `TypeError` polish | Kept open; load-bearing but deferable. §8.1. |
| Q8 | Backend modularity / "quickcheck-style mock" | The "mock" was a stray reference (a testing backend that intercepts the C call with arbitrary values instead of actually calling C — useful for property-testing a high-level wrapper, but not load-bearing). The actual requirement: PP + TH backends, both trivially supported. §6.5. |

[issue-8]: https://github.com/well-typed/hs-bindgen/issues/8
