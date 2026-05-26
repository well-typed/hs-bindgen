# High-level binding DSL — core design

This document is the **core design only**, with no defaults, no
auto-inference, and no concession to round 1's `hl`. Each per-position
marshaller is a value the user picks; each spec is built explicitly;
each conversion is local. The earlier drafts (`PROPOSAL.md` for round 1,
`PROPOSAL_v2.md` for the iteration that explored Tier-2 defaults and
walking classes) hold the workflow audit trail.

## 1. The problem in three lines

`hs-bindgen` emits a low-level binding like `parse_int :: PtrConst CChar
-> Ptr CInt -> IO CInt`. The user wants a wrapper of type
`String -> IO (Int, Int)`. Round 2 ships a value-level DSL for
specifying the wrapper declaratively, per call.

## 2. Marshallers — record types that carry the slot at the type level

A marshaller is a value-level conversion between Haskell and C. Three
record types, one per role. Their second type parameter is the **slot**:
the C-side shape this marshaller fills.

```haskell
data InMarshaller hs slot = InMarshaller
  { withIn :: forall r. hs -> (slot -> IO r) -> IO r }

data OutMarshaller hs slot = OutMarshaller
  { withOutM :: forall r. (slot -> IO r) -> IO (hs, r) }

data ResMarshaller c hs = ResMarshaller
  { runResM :: c -> IO hs }
```

`InMarshaller` is bracket-shaped: it produces the C slot, runs the
continuation, and tears down. `OutMarshaller` is its dual on the result
side: it allocates a slot, runs the C call, and peeks. `ResMarshaller`
converts the C return value (effectfully — any `c -> IO hs`).

For one Haskell value spanning multiple consecutive C arguments
(c2hs's `&` pattern), the slot is a small data constructor that names
the arity:

```haskell
data MultipleArgs2 a b     = MA2 a b
data MultipleArgs3 a b c   = MA3 a b c
data MultipleArgs4 a b c d = MA4 a b c d
```

These never appear in the user's high-level signatures; they only
appear inside marshaller types — e.g.
`InMarshaller ByteString (MultipleArgs2 (PtrConst CChar) CSize)`.

Concrete marshaller examples:

```haskell
withCStringIn :: InMarshaller String (PtrConst CChar)
withCStringIn = InMarshaller $ \s k -> withCString s (k . PtrConst.unsafeFromPtr)

useAsCStringLenIn :: InMarshaller ByteString (MultipleArgs2 (PtrConst CChar) CSize)
useAsCStringLenIn = InMarshaller $ \bs k ->
  BS.useAsCStringLen bs $ \(p, n) ->
    k (MA2 (PtrConst.unsafeFromPtr p) (fromIntegral n))

allocaIntOut :: OutMarshaller Int (Ptr CInt)
allocaIntOut = OutMarshaller $ \k -> alloca $ \p -> do
  r <- k p
  v <- peek p
  pure (fromIntegral v, r)

allocaBufOut :: forall a. Storable a
             => Int  -- capacity
             -> OutMarshaller (IncompleteArray a)
                              (MultipleArgs2 (Ptr a) (Ptr CSize))
allocaBufOut cap = OutMarshaller $ \k ->
  allocaBytes (cap * sizeOf @a undefined) $ \pBuf ->
    alloca $ \pLen -> do
      poke pLen (fromIntegral cap)
      r <- k (MA2 pBuf pLen)
      n <- peek pLen
      arr <- IA.peekArray (fromIntegral n) (IA.toPtr pBuf)
      pure (arr, r)
```

## 3. `Spec lo hi` — a binding spec, reified as a GADT

A binding spec is a value of type `Spec lo hi`. It says "this is a
recipe for turning a low-level callable of type `lo` into a wrapper of
type `hi`". The type indices are not phantom: they track exactly what
has been consumed from the C signature and what shape is being built on
the Haskell side.

```haskell
data Spec lo hi where
  Done    :: ResMarshaller c hs
          -> Spec (IO c) (IO hs)
  Discard :: Spec (IO c) (IO ())
  In      :: (Spread slot lo lo', ThreadIn  slot    rest)
          => InMarshaller hs slot
          -> Spec lo' rest
          -> Spec lo (hs -> rest)
  Out     :: (Spread slot lo lo', ThreadOut slot hs rest rest')
          => OutMarshaller hs slot
          -> Spec lo' rest
          -> Spec lo  rest'
```

Read the constructors top-down:

- **`Done`** closes a spec by converting the C return. It only fits
  when the inner C signature has reached `IO c`.
- **`Discard`** closes a spec by ignoring the C return. The Hs side
  becomes `IO ()`.
- **`In`** wraps an inner spec by adding *one input slot* at the head
  of the C signature. `Spread` (§4.1) consumes the slot's C args.
  `ThreadIn` (§4.2) walks past any Hs args the inner spec has
  accumulated and fires the input bracket at the innermost `IO`.
- **`Out`** wraps an inner spec by adding *one output slot* at the
  head of the C signature. `Spread` consumes the slot's C args.
  `ThreadOut` (§4.2) — the *dual* of `ThreadIn` — walks the same way
  but prepends the output's `hs` value to the innermost `IO`'s
  payload, so the output contributes to the result tuple.

All three constraints come into scope automatically when the pattern
match on the constructor fires in the runner. `In` and `Out` are
structurally symmetric — same `Spread`, dual `ThreadIn`/`ThreadOut`,
both wrapping one inner spec — apart from `Out`'s output-tuple
contribution.

Pattern: every constructor adds **one** position at the head, and the
inner spec already knows the shape of everything after that position.
There is no walking past prior Hs args, because at the moment a
constructor fires, no prior Hs args exist *in this scope* — they all
live in the outer spec that wraps this one.

### 3.1 One property the GADT encodes

The `Out` constructor uses `ThreadOut` to walk through the inner
spec's Hs args before prepending its component to the innermost `IO`.
This means `input` and `output` may appear in /any/ order in the
chain — including a c2hs-style `out, in, out` interleaving. The
result type, however, is still a /right-associative nested tuple/:
each `Out` prepends one component to the inner `IO`'s payload, so
*N* outputs plus a `result` produce
`IO (h_1, (h_2, ..., (h_n, c)))`. The user destructures at the call
site with `(a, (b, c)) <- ...` or by composing with a small
flattener. A `MultipleResults<N>` data family (§9) would flatten the
shape but is not in the core design.

## 4. Type classes — what they do, and why they exist

Three classes form the substrate's only type-class machinery:
`Spread`, `ThreadIn`, and `ThreadOut`. `Spread` is reused from round
1 (`hs-bindgen-runtime/src/HsBindgen/Runtime/HighLevel/Call.hs:37-51`);
`ThreadIn` and `ThreadOut` are introduced for the spec design and
are dual.

Before the definitions, the *why*. Combinators chain right-to-left,
which is what gives the spec its C-arg-order reading at the call
site. The consequence is that by the time an `input` or `output`
fires at run time, the inner spec may have already accumulated some
number of Hs arguments — its high-level type can have the shape

  ``arg1 -> arg2 -> ... -> argN -> IO X``

with any number of leading arrows. A marshaller's bracket, however,
has the CPS shape ``forall r. ... -> (slot -> IO r) -> IO r`` — its
continuation expects to *end* in `IO r`, not in `arg -> ... -> IO
r`. So the bracket cannot fire immediately; it must defer until the
user has supplied those `arg`s. The classes `ThreadIn` and
`ThreadOut` are the type-directed structural recursion on `(->)`
that walks past those args and fires the bracket at the innermost
`IO`.

### 4.1 `Spread` — apply a slot to a curried function

`Spread` applies a `slot` value to the head of a curried C function,
consuming one C arg (single-slot fallback) or N C args
(`MultipleArgs<N>`):

```haskell
class Spread slot lo lo' | slot lo -> lo' where
  spread :: slot -> lo -> lo'

instance {-# OVERLAPPABLE #-} Spread t (t -> lo) lo where
  spread t f = f t
instance Spread (MultipleArgs2 a b)     (a -> b -> lo)           lo where
  spread (MA2 a b)     f = f a b
instance Spread (MultipleArgs3 a b c)   (a -> b -> c -> lo)      lo where
  spread (MA3 a b c)   f = f a b c
instance Spread (MultipleArgs4 a b c d) (a -> b -> c -> d -> lo) lo where
  spread (MA4 a b c d) f = f a b c d
```

`Spread` answers "given a slot value, how many C args does it
consume from the head of `lo`?". The answer is one for single types
(the OVERLAPPABLE fallback) and N for `MultipleArgs<N>`.

### 4.2 `ThreadIn` and `ThreadOut` — defer a bracket past prior Hs args

Both classes implement the same idea: structural recursion on
`(->)` that introduces a fresh lambda for each Hs arg the inner
spec has accumulated, and fires the bracket once it reaches the
innermost `IO`. They differ only in what the bracket returns.

#### `ThreadIn`

```haskell
class ThreadIn slot g where
  threadIn :: (forall r. (slot -> IO r) -> IO r)
           -> (slot -> g) -> g
```

How to read each type argument:

- `slot` — the C-side value the bracket produces. For a `String`
  input via `withCStringIn`, `slot = PtrConst CChar`.
- `g` — the inner spec's high-level shape. Always ends in some
  `IO X`; the leading `arg ->` arrows (if any) come from prior
  inputs that the inner spec has already wrapped.

How to read `threadIn br body`:

- `br` is the bracket. It's a CPS computation: "give me a
  continuation that wants a slot; I'll allocate the slot, call your
  continuation with it, and tear down when your continuation
  returns."
- `body :: slot -> g` is what to do once the slot is in hand.
- The output is `g` — unchanged. `ThreadIn` defers when the bracket
  fires; it does not change the type the spec produces.

```haskell
-- Base case: g is `IO r`. Fire the bracket now.
instance ThreadIn slot (IO r) where
  threadIn br f = br f

-- Step case: g is a function. Introduce a fresh lambda for the arg;
-- recurse on the remaining shape with the arg already supplied.
instance ThreadIn slot rest
       => ThreadIn slot (arg -> rest) where
  threadIn br f = \arg -> threadIn br (\s -> f s arg)
```

#### `ThreadOut`

```haskell
class ThreadOut slot hs g g' | slot hs g -> g' where
  threadOut :: (forall r. (slot -> IO r) -> IO (hs, r))
            -> (slot -> g) -> g'
```

How to read each type argument:

- `slot`, `g` — same as `ThreadIn`.
- `hs` — the Hs-side value the bracket produces *alongside* the
  continuation's result. For an `Int` out-parameter via `peekOut`,
  `hs = Int`.
- `g'` — what `g` becomes once `hs` is prepended to its innermost
  `IO`'s payload. `ThreadOut` *does* change the result type by one
  tuple slot. The functional dependency `slot hs g -> g'` says GHC
  computes `g'` from the other three; the user never writes it.

How to read `threadOut br body`:

- `br` is the output bracket: "give me a continuation; I'll
  allocate the slot, call the continuation with it, then peek and
  pair the peeked value with the continuation's result."
- `body :: slot -> g` is what to do once the slot is in hand.
- The output is `g'` — `g` with `hs` prepended at the innermost
  `IO`. The output value joins the result tuple.

```haskell
-- Base case: g is `IO r`. Fire the bracket; the result is `IO (hs, r)`.
instance ThreadOut slot hs (IO r) (IO (hs, r)) where
  threadOut br f = br f

-- Step case: g is a function. Same recursion as ThreadIn, but the
-- function's result grows by one tuple slot at the innermost IO.
instance ThreadOut slot hs rest rest'
       => ThreadOut slot hs (arg -> rest) (arg -> rest') where
  threadOut br f = \arg -> threadOut br (\s -> f s arg)
```

#### Reading them as a pair

| concern | `ThreadIn` | `ThreadOut` |
|---|---|---|
| recursion on `(arg -> rest)` | walks past `arg` | walks past `arg` |
| base case fires at | `IO r` | `IO r` |
| bracket shape | `(slot -> IO r) -> IO r` | `(slot -> IO r) -> IO (hs, r)` |
| result type effect | none (`g` → `g`) | prepend `hs` at innermost `IO` (`g` → `g'`) |

Three rows identical, one differs. The differing row is the whole
purpose of `ThreadOut`: it threads the output's `hs` to the result
tuple at the right depth, while keeping the bracket lifetime
correct.

`ThreadIn` is the same shape as round 1's `Thread`; we re-introduce
it under a name that names the dual. Round 1's `Thread` stays in
`Call.hs` for use by the `hl` elaborator; the duplication is
intentional and minor (about five lines).

### 4.3 Step-by-step examples

Two examples, each walking from the source spec to the IO action
`bind` ultimately produces. The first uses `ThreadIn`; the second
uses `ThreadOut` and demonstrates the case where an output appears
*before* an input in the spec.

#### Example 1: `ThreadIn` — one input wraps another

C signature:

```c
int strncmp(const char *s1, const char *s2, size_t n);
```

Wrapper:

```haskell
hsStrncmp :: String -> ByteString -> IO Int
hsStrncmp = bind ( input  withCStringIn
                 $ input  useAsByteStringLenIn
                 $ result (pureRes fromIntegral)
                 ) strncmp
```

**Construction.** Read the spec innermost-first.

```
1) result (pureRes fromIntegral)
     :: Spec (IO CInt) (IO Int)

2) input useAsByteStringLenIn $ <1>
     ByteString consumes two C args via `useAsByteStringLenIn`'s
     MultipleArgs2 slot. ThreadIn fires its base case (inner hi is
     `IO Int`).
     :: Spec (PtrConst CChar -> CSize -> IO CInt) (ByteString -> IO Int)

3) input withCStringIn $ <2>
     String consumes one C arg. ThreadIn now sees an inner hi of
     `ByteString -> IO Int` — a function type. Its step instance
     fires, introducing a `\bs ->` lambda.
     :: Spec (PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt)
                                          (String -> ByteString -> IO Int)
```

**Runtime.** At step 3, `bind` produces this function:

```
bind <step3> strncmp
  = \s -> threadIn (withIn withCStringIn s)
                   (\slot -> bind <step2> (spread slot strncmp))
```

The inner body has type `PtrConst CChar -> (ByteString -> IO Int)`
— take a slot, then a `ByteString`, then run. `threadIn`'s step
instance fires once (one `\bs ->` introduced), then the base case
fires when the recursion reaches `IO Int`:

```
  = \s -> \bs -> withIn withCStringIn s $ \slot ->
                   bind <step2> (spread slot strncmp) bs
  = \s -> \bs -> withCString s $ \pStr ->
                   bind <step2> (strncmp (PtrConst.unsafeFromPtr pStr)) bs
```

The `withCString` bracket fires *after* the user supplies both `s`
and `bs`. The C-side pointer is valid for the duration of the inner
spec's evaluation and freed when it returns — exactly the lifetime
the FFI call needs.

#### Example 2: `ThreadOut` — output is the *first* C argument

C signature (output first, input second):

```c
int compute(int *out, int n);   /* writes n*2 to *out, returns 0 */
```

Wrapper:

```haskell
hsCompute :: Int -> IO (Int, Int)
hsCompute = bind ( output (peekOut (\(CInt v) -> pure (fromIntegral v)))
                 $ input  (pureIn  (CInt . fromIntegral))
                 $ result (pureRes fromIntegral)
                 ) compute
```

The spec mirrors C-arg order: `output` first (for `int *out`),
`input` second (for `int n`), `result` last (for the status). The
user's wrapper type is `Int -> IO (Int, Int)` — the `Int` argument
is the input; the result tuple holds the out-parameter value and
the status, in that order.

**Construction.** Read innermost-first.

```
1) result (pureRes fromIntegral)
     :: Spec (IO CInt) (IO Int)

2) input (pureIn (CInt . fromIntegral)) $ <1>
     Int → CInt is pure. ThreadIn's base case fires (inner hi is `IO Int`).
     :: Spec (CInt -> IO CInt) (Int -> IO Int)

3) output (peekOut ...) $ <2>
     Allocates a `Ptr CInt`, runs the call, peeks the result, converts
     to Int. ThreadOut now sees an inner hi of `Int -> IO Int` — a
     function type. Its step instance fires, introducing a `\arg ->`
     lambda; the inner ThreadOut at `IO Int` then fires its base case,
     and the result type grows by one slot.
     :: Spec (Ptr CInt -> CInt -> IO CInt) (Int -> IO (Int, Int))
```

**Runtime.** `bind` produces:

```
bind <step3> compute
  = threadOut (withOutM (peekOut k))
              (\slot -> bind <step2> (spread slot compute))
  where k (CInt v) = pure (fromIntegral v)
```

The inner body has type `Ptr CInt -> (Int -> IO Int)` — take a
slot, then an `Int`, then run. `threadOut`'s step instance fires
once (one `\arg ->` introduced), then the base case fires at the
innermost `IO`:

```
  = \arg -> threadOut (withOutM (peekOut k))
                      (\slot -> bind <step2> (spread slot compute) arg)
  = \arg -> withOutM (peekOut k)
              (\slot -> bind <step2> (spread slot compute) arg)
  = \arg -> alloca $ \p -> do
              r  <- (\slot -> bind <step2> (spread slot compute) arg) p
              c  <- peek p
              hs <- k c
              pure (hs, r)
  = \arg -> alloca $ \p -> do
              r  <- bind <step2> (compute p) arg     -- runs the input + result
              c  <- peek p                            -- read out-param
              hs <- k c                               -- convert
              pure (hs, r)
```

The key observation: `alloca $ \p -> ...` fires *before* the
user's `Int` is consumed inside the inner spec. The pointer is
allocated, the C call (via the inner spec) runs and writes into it,
and the value is peeked back — all under the same `alloca` bracket.
The result tuple is then `(peeked-value, status)`, in that order,
because `ThreadOut` prepended the output's `Int` to the inner
spec's `IO Int`.

For `hsCompute 7`: `alloca` a `Ptr CInt`, `compute p (CInt 7)`
writes `CInt 14` to `*p` and returns `0`, peek gives `CInt 14`,
convert gives `14`, pair with the status: `IO (14, 0)`.

## 5. `bind` — running a spec

One interpreter. Pattern-matches the GADT and threads the C callable
through each layer:

```haskell
bind :: Spec lo hi -> lo -> hi
bind (Done (ResMarshaller k))     cFn = cFn >>= k
bind Discard                      cFn = () <$ cFn
bind (In  (InMarshaller m)  spec) cFn = \hs -> threadIn  (m hs) $ \slot ->
                                                bind spec (spread slot cFn)
bind (Out (OutMarshaller m) spec) cFn =        threadOut  m     $ \slot ->
                                                bind spec (spread slot cFn)
```

`In` and `Out` are exactly symmetric: each spreads its slot into the
head of `cFn`, threads its bracket through whatever Hs args the
inner spec has already accumulated, and recurses on the spec body.
The only differences are the bracket's shape — `In`'s bracket
returns its continuation's result; `Out`'s additionally prepends
its `hs` value at the innermost `IO` — and the choice of threader
(`threadIn` vs `threadOut`). The pattern match on the GADT
constructor brings the `Spread` and `ThreadIn`/`ThreadOut`
constraints into scope, which is what lets `spread`, `threadIn`,
and `threadOut` typecheck inside each branch.

## 6. Smart constructors and `$`-chaining

The GADT constructors `Done`, `Discard`, `In`, `Out` are usable
directly. Lowercase aliases give the same right-associative pipeline
the user wrote in PROPOSAL_v2:

```haskell
result     = Done
discardRes = Discard
input      = In
output     = Out
```

User code reads top-down in C-arg order, because `$` is
right-associative:

```haskell
hsParseInt :: String -> IO (Int, Int)
hsParseInt = bind ( input  withCStringIn      -- 1st C arg: const char* ← String
                  $ output allocaIntOut       -- 2nd C arg: int*       → Int
                  $ result (pureRes fromIntegral)
                  )
                  parse_int
  where
    pureRes f = ResMarshaller (pure . f)
```

Type trace (bottom-up — the way construction proceeds):

```
result (pureRes fromIntegral)        :: Spec (IO CInt) (IO Int)
output allocaIntOut (...)            :: Spec (Ptr CInt -> IO CInt) (IO (Int, Int))
input withCStringIn (...)            :: Spec (PtrConst CChar -> Ptr CInt -> IO CInt)
                                              (String -> IO (Int, Int))
bind ... parse_int                   :: String -> IO (Int, Int)
```

The result for a *single* `output` followed by a `result` is a flat
2-tuple `IO (Int, Int)`. With *two or more* outputs, each `Out`
prepends one component to the inner `IO`'s payload, so the result is
a /right-associative nested tuple/ — `IO (a, (b, c))` for two
outputs plus a result. `output` may appear anywhere in the chain
(including between two `input`s); the corresponding output value
joins the result tuple at the same nesting depth as the `output`
position in the spec. See §9 for the deferred `MultipleResults<N>`
polish that would flatten the shape.

## 7. Worked examples

The six C patterns from `PROPOSAL.md` §2 plus the two effectful cases.

```haskell
-- strlen
hsStrlen :: String -> IO Int
hsStrlen = bind ( input  withCStringIn
                $ result (pureRes fromIntegral) ) strlen

-- do_thing(str, len) with the `&` pattern, via per-call marshaller
hsDoThing :: ByteString -> IO Int
hsDoThing = bind ( input  useAsByteStringLenIn
                 $ result (pureRes fromIntegral) ) do_thing

-- parse_int(s, out) with declarative output
hsParseInt :: String -> IO (Int, Int)
hsParseInt = bind ( input  withCStringIn
                  $ output allocaIntOut
                  $ result (pureRes fromIntegral) ) parse_int

-- srp6 — five inputs, one buffer output, one status return
srp6Step1 :: ServerSession -> Verifier -> GroupId -> HashId -> RNG -> IO B
srp6Step1 ses ver gid hid rng = do
  cap <- srp6GroupSize gid
  (arr, _status) <- bind ( input  sessionIn
                         $ input  verifierIn         -- 2 slots
                         $ input  groupIdIn
                         $ input  hashIdIn
                         $ input  rngIn
                         $ output (allocaBufOut (fromIntegral cap))  -- 2 slots
                         $ result (pureRes fromIntegral)
                         )
                         botan_srp6_server_session_step1
                         ses ver gid hid rng
  pure (B arr)

-- llMalloc with effectful conversion
hlMalloc :: Int -> IO ByteArray
hlMalloc = bind ( input  (pureIn fromIntegral)
                $ result (ioRes helper) ) llMalloc

-- llMalloc' (out-pointer plus effectful peek)
hlMalloc' :: Int -> IO ByteArray
hlMalloc' n = fst <$> bind ( input  (pureIn fromIntegral)
                           $ output (peekOut helper)
                           $ discardRes
                           )
                           llMalloc' n

-- Per-call String marshallers — Edsko's two-String case
foo :: String -> String -> IO Int
foo = bind ( input  withCStringIn         -- first  String: withCString
           $ input  useAsCStringLenInI    -- second String: withCStringLen
           $ output (peekOut (pure . fromIntegral))
           $ discardRes
           ) cFoo
```

The standard library of marshallers (`pureRes`, `ioRes`, `pureIn`,
`peekOut`, `withCStringIn`, `useAsCStringLenIn`, `useAsByteStringLenIn`,
`allocaBufOut`, `allocaIntOut`, `funPtrIn`, `nullableIn`, ...) is plain
Haskell — values constructed once and reused.

## 8. GADT vs. function combinators — the trade-off

The same user-facing pipeline can be implemented two ways:

**(a) Function combinators on a transparent newtype.**

```haskell
newtype Spec lo hi = Spec { runSpec :: lo -> hi }

result :: ResMarshaller c hs -> Spec (IO c) (IO hs)
result (ResMarshaller k) = Spec (>>= k)

input :: Spread slot lo lo'
      => InMarshaller hs slot -> Spec lo' rest -> Spec lo (hs -> rest)
input (InMarshaller m) (Spec k) =
  Spec $ \cFn hs -> m hs $ \slot -> k (spread slot cFn)

-- ... output, discardRes, similarly
bind = runSpec
```

**(b) GADT (this proposal).**

Equivalent user-facing API; the substrate is a reified GADT with one
runner.

The two differ in *what a Spec is at runtime*:

- (a): an opaque closure. The only way to use it is to apply it to a
  low-level callable.
- (b): a syntax tree. You can pattern-match on it, fold over it, derive
  a different interpretation, or analyse it without running.

For the present milestone (hand-written wrappers), (a) is shorter to
implement and equally ergonomic for the user. For Milestone 3 (the
generator emits high-level wrappers automatically), reification is
worth it: a generated Spec value can be inspected, validated, and
emitted as commented source, none of which is possible with (a)'s
closures. The cost of (b) is small — a GADT definition plus a
pattern-matching runner, in place of four combinator function bodies
that wrap closures.

Recommendation: ship (b). The substrate is small enough that the
extra declaration is not a real cost; the option to grow analysis
passes later is worth keeping.

## 9. What this proposal explicitly omits

- **Type-class-driven defaults** (`HasInDefault`, `HasResDefault`,
  `autoIn`, `autoRes`). The user picks marshallers by name at every
  call.
- **Tier-3 inference** (`hl c_func`). Round-1's machinery is unchanged
  in its module; it can be revisited later as an additional entry
  point.
- **Generator-side support.** Milestone 3 territory; the GADT here is
  the natural target.
- **`MultipleResults<N>` flat-tuple wrapping** of the result. We use
  right-associative nested pairs in the `IO`; for one or two outputs
  this is fine, for three or more it is mildly awkward. The pattern
  can be added later as a final `flatten` combinator that pattern-
  matches over the GADT.

## 10. The `Spread` instance set is still per-arity

`Spread` has one instance per `MultipleArgs<N>` arity (plus the
single-arg fallback). This is intentional: each instance does one
N-uncurrying, which GHC inlines into the runner. Round 1 had the
same shape with tuple-typed instances; we rename to
`MultipleArgs<N>` for documentation, not for any change in mechanism.

The user only encounters `MultipleArgs<N>` if they author a custom
multi-arg marshaller. Built-in multi-arg marshallers (e.g.
`useAsCStringLenIn`) hide the constructor inside their bodies.

## 11. Migration from round 1

- `hs-bindgen-runtime/src/HsBindgen/Runtime/HighLevel/ToC.hs`,
  `FromC.hs`, `Call.hs`, `Result.hs`, `Prelude.hs` — round 1's modules
  remain in place. Their classes (`ToC`, `FromC`, `HighLevel`) stay as
  the optional inference layer that may be revisited.

- A new module `HsBindgen.Runtime.HighLevel.Spec` holds:
  - The `Spec` GADT,
  - The marshaller record types,
  - The `bind` runner,
  - The smart constructors,
  - The standard marshaller library.

- The `Spread` class is shared between the two layers (imported by
  `Spec.hs` from `Call.hs`); nothing about it changes.

- `ThreadIn` is the same shape as round 1's `Thread` but is defined
  locally in `Spec.hs` under a name that makes the duality with
  `ThreadOut` visible. Round 1's `Thread` stays where it is.

- Round-1 result-side helpers `withOut` and `withBuf`
  (`hs-bindgen-runtime/src/HsBindgen/Runtime/HighLevel/Result.hs:31-59`)
  are deprecated with pointers to the new `output (allocaOut …)` and
  `output (allocaBufOut …)` combinators.

That is the entire design.
