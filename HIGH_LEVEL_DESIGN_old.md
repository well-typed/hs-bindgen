# High-level bindings for `hs-bindgen` — design proposal

> Canonical design proposal for [hs-bindgen issue #8][issue-8]. Companion
> document: [`ISSUE_ANALYSIS.md`](./ISSUE_ANALYSIS.md), which contains the
> workflow audit trail (plans considered, evaluator review, blast radius,
> citation verification).
>
> **Status**: design proposal, awaiting team review. Implementation has not
> started.
>
> **Audience**: `hs-bindgen` contributors and the Well-Typed team. Assumes
> familiarity with the codebase (`HasFFIType`, `IsArray`, `IncompleteArray`,
> the runtime module hierarchy) and Haskell idioms (deriving-via,
> type families, GADT-flavoured `IO` continuations).

[issue-8]: https://github.com/well-typed/hs-bindgen/issues/8

---

## Table of contents

1. [Problem and context](#1-problem-and-context)
2. [Worked example](#2-worked-example)
3. [State of the art](#3-state-of-the-art)
4. [Where `hs-bindgen` is today](#4-where-hs-bindgen-is-today)
5. [Suggested design](#5-suggested-design)
6. [Caveats](#6-caveats)

---

## 1. Problem and context

### 1.1 What `hs-bindgen` does today

`hs-bindgen` reads a C header and produces a **low-level** Haskell binding
that mirrors the C API one-for-one. Pointers stay as `Ptr`, integers as
`CInt`, booleans as `CBool`, strings as `Ptr CChar` (or `PtrConst CChar`
for `const char *`). The runtime support library
(`hs-bindgen-runtime`) provides the glue: `IsArray`, `IncompleteArray`,
`PtrConst`, `CEnum`, `HasCField`, `HasFFIType`, `ToFunPtr`,
`FromFunPtr`, …

This is the right floor. It is *not* what most users want at the
*ceiling*: they want a Haskell-native API where strings are `String` or
`Text` or `ByteString`, bools are `Bool`, nullable pointers are `Maybe`,
arrays are `Vector` or `IncompleteArray` with an obvious value-level
shape, and resource lifetimes are managed by `bracket`-shaped
combinators. Today, users get there by hand-writing a wrapper module on
top of the generated bindings.

This pattern — *low-level package + high-level wrapper on top* — is so
common that essentially every serious Haskell binding library ships two
layers: `OpenGLRaw`/`OpenGL`, `vulkan-api`/`vulkan`, `bindings-GLFW`/`GLFW-b`,
`botan-low`/`botan`. The Rust ecosystem has the same convention (`*-sys` +
the idiomatic crate). The roadmap (`manual/roadmap.md:76-83`) calls this
out explicitly as **Milestone 2.5**:

> When hand-writing high-level bindings, there are undoubtedly a lot of
> patterns that emerge. We should capture these as Haskell functions or
> type classes and release this as a separate library `hs-bindgen-runtime`.

This document is about exactly that capture: a small, compositional,
type-class-based vocabulary that turns the high-level wrapper module from
a 320-line `withCString` / `alloca` / `peek` ladder into a 120-line file
where every line carries domain meaning.

### 1.2 Why this is worth doing

Three reasons.

**One**: the patterns *exist*. They are repeatable: every C-with-Haskell
binding pairs the same handful of marshalling shapes (pointer + length;
out-parameter; init/destroy; NULL ↔ Maybe; pre-allocated buffer with
length cap; …). When the patterns are spelled out by hand at every call
site, the result is hard to audit, hard to maintain, and depressing to
read. When they live in a class, each pattern has a name, one
implementation, one test.

**Two**: a vocabulary unlocks generation later. Milestone 3 is "generate
the high-level layer too". That milestone is a non-starter without a
target vocabulary — the generator has to emit *something*. If the
vocabulary is good and lives in `hs-bindgen-runtime`, the generator's job
is mechanical. If there is no vocabulary, the generator either invents
one (bad — it must be designed) or hard-codes the patterns directly into
the codegen (worse — opaque, untestable, non-composable).

**Three**: the design is its own deliverable. Even before generation
exists, a user who today writes `srp6_server_session_step1` by hand
(`examples/botan/hs-project/src/Botan.hs:109-136`) can rewrite it as a
single line `hl srp6_server_session_step1_raw` and get a five-fold
reduction in glue. The library *is* the milestone.

### 1.3 What we are not solving here

- We are not generating high-level bindings. The user writes the
  high-level signature; the library supplies the marshalling glue.
- We are not changing the low-level generator output. This proposal sits
  on top.
- We are not designing for variadics; `hs-bindgen` already chooses not
  to support them (`manual/low-level/translation/functions.md:538-562`).
- We are not building error-handling-as-exceptions into the default
  marshalling. Each high-level binding declares its own error
  convention; a generic `Result` instance can layer it in if desired.
  See open question Q2 in `ISSUE_ANALYSIS.md` §7.

---

## 2. Worked example

Three C functions, in increasing complexity. For each: the C signature,
the low-level binding `hs-bindgen` generates today, the high-level
wrapper a user must currently write, and the wrapper they *would* write
with the proposed API.

### 2.1 `strlen` — a one-arg one-result function

The "hello world" case. Pure single-slot, pure single-result.

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

What they would write with the proposed API:

```haskell
hsStrlen :: String -> IO Int
hsStrlen = hl strlen
```

Three building blocks resolve in the background:

- `Marshal String` (with `Foreign String = PtrConst CChar`,
  `withMarshal = withCString . castPtr ...`) — the L1 instance.
- `Marshal Int` (pure conversion from `CSize` via `fromIntegral`).
- One generic `hl :: Call hi lo => lo -> hi` — the type-class machinery
  picks `Marshal` instances for each argument and the result.

### 2.2 `do_thing(str, len)` — one Haskell argument, two C arguments

The c2hs `&` case. This is the central pattern the user's prompt
called out.

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
    fromIntegral <$>
      do_thing (PtrConst.unsafeFromPtr p) (fromIntegral n)
```

What they would write with the proposed API:

```haskell
hsDoThing :: PtrLen ByteString -> IO Int
hsDoThing = hl do_thing
```

The crucial new piece is the `PtrLen` newtype, which signals "this one
Haskell argument expands into two C arguments". Its `Argument` instance
binds the two slots. The `Call` class threads them through. The result
unwraps the `CInt` into `Int` via the same `Marshal` instance as before.

This is the pattern that motivated the multi-slot tier of the design.
Without it, the user has to either invent a `(ByteString, ())` newtype
plus an `Argument` instance for it (verbose) or fall back to the
hand-written wrapper.

### 2.3 `get_size(out)` — out-parameter result

A function whose return value is delivered by writing into a
caller-allocated pointer.

```c
int get_size(size_t *out);
```

What `hs-bindgen` generates:

```haskell
get_size :: Ptr CSize -> IO CInt
```

What a user writes today:

```haskell
hsGetSize :: IO Int
hsGetSize = alloca $ \resPtr -> do
  status <- get_size resPtr
  when (status /= 0) $ fail "get_size failed"
  fromIntegral <$> peek resPtr
```

What they would write with the proposed API:

```haskell
hsGetSize :: IO (Out Int)
hsGetSize = hl get_size
```

The `Out` wrapper signals "this Haskell result was reconstructed from a
C out-parameter". A `Result (Out a)` instance handles the
allocate-then-peek dance. The user opts out of `Out`'s wrapping with an
`unwrapOut` accessor if the wrapper newtype gets in the way.

(Note we are explicitly *not* turning the non-zero status into an
exception by default. That is a per-binding choice the user makes by
layering a `Throws` wrapper, or by leaving the status visible. See §1.3
and §6.)

### 2.4 The combined picture — `srp6_server_session_step1`

For the full picture, here is what the design does to the most complex
example in the project's `examples/` directory
(`examples/botan/hs-project/src/Botan.hs:109-136`). The C function:

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

What the user writes today — **27 lines** of `withCString`/`allocaBytes`/
`alloca`/`poke`/`peek`/`fromIntegral`/`peekArray`/`throwErrnoIfNegative`:

```haskell
srp6ServerSessionStep1 :: ServerSession -> Verifier -> GroupId -> HashId -> RNG
                       -> IO B
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
      throwErrnoIfNegative "botan_srp6_server_session_step1" $
        botan_srp6_server_session_step1 s
          (PtrConst.unsafeFromPtr verifierPtr) verifierLen
          (PtrConst.unsafeFromPtr groupIdPtr)
          (PtrConst.unsafeFromPtr hashIdPtr)
          rngObj bPtr bLenPtr
      bLen <- peek bLenPtr
      B <$> IA.peekArray (fromIntegral bLen) (IA.toPtr bPtr)
```

What they would write with the proposed API:

```haskell
srp6ServerSessionStep1
  :: ServerSession
  -> PtrLen Verifier
  -> GroupId -> HashId
  -> RNG
  -> CSize                  -- buffer capacity
  -> IO (Buf Word8)
srp6ServerSessionStep1 = hl botan_srp6_server_session_step1
```

The instances that pull this together:

- `Marshal ServerSession`, `Marshal GroupId`, `Marshal HashId`,
  `Marshal RNG` — one-liner `deriving Marshal via Pure ...` per
  domain newtype.
- `Argument (PtrLen Verifier)` — newtype-derived from `Argument (PtrLen
  (IncompleteArray Word8))` which lives in the library.
- `Marshal CSize` — already in the library (`Pure CSize`).
- `Result (Buf Word8)` — in the library; allocates `Ptr Word8` at the
  user-supplied capacity, allocates `Ptr CSize`, pre-pokes the capacity,
  peeks both after the call.

User-written code in the wrapper module: roughly *5 lines* of `deriving`
clauses plus one `Argument` line, replacing 27 lines of mechanical glue.

---

## 3. State of the art

The tools and techniques most relevant to a compositional, type-class-based
high-level binding library.

### 3.1 `c2hs` — the existing Haskell tool

`c2hs` is a preprocessor that reads C headers and rewrites `{# fun #}` /
`{# enum #}` / `{# pointer #}` / `{# get #}` / `{# set #}` directives in
a `.chs` file into a Haskell module with `foreign import` declarations
and matching marshalling wrappers. We want it because it has been
solving exactly our problem for ~25 years. We do *not* want to copy it.

A small example. Consider:

```c
int do_thing(const char *str, int len);
int get_size(size_t *out);
```

In `c2hs`:

```haskell
{#fun do_thing as ^
   { withCStringLen* `String'& } -> `Int' #}

{#fun get_size as ^
   { alloca- `Int' peekIntConv* } -> `Int' #}
```

The first declaration says: take one Haskell `String` (the back-ticked
`String'`), use `withCStringLen` as its `in` marshaller (the `*` after a
function name means "an IO bracket"), and splat the resulting
`(Ptr CChar, Int)` across two C arguments (the trailing `&` on the type).
Generated Haskell:

```haskell
doThing :: String -> IO Int
doThing s = withCStringLen s $ \(p, n) ->
              fromIntegral <$> do_thing'_ p (fromIntegral n)
```

The second declaration says: this argument is suppressed from the
Haskell signature (`alloca-` — the trailing `-` on the marshaller means
"don't expose it"), the marshaller allocates a `size_t*`, and after the
call we `peekIntConv*` it into an `Int` (the trailing `*` again being IO,
appended to the Haskell result tuple). Generated:

```haskell
getSize :: IO Int
getSize = alloca $ \p -> get_size'_ p >> fromIntegral <$> peek p
```

The full grammar has ~6 modifiers: `*` (IO bracket), `-` (suppress),
`&` (one-Hs-to-two-C), `*-` (effect-only, like `throwErrnoIf*-`),
`+`/`+S`/`+N` (allocate scratch and append to result), `%` (struct by
value via a generated C wrapper). The default-marshaller table picks a
sensible marshaller per `(Hs type, C type)` pair so users typically write
just `String'` and get `withCString*`/`peekCString*` for free.

**What c2hs gets right.** Three things stand out.

First, its decomposition of an argument position into *(in marshaller,
out marshaller, modifiers)* is correct. Every binding boils down to this
shape, regardless of language.

Second, the `&` modifier names a real problem (`1 Hs → N C slots`) and
solves it for `N=2`. We need a typed Haskell version of the same thing.

Third, the `*-` modifier (effectful out marshaller whose value is
discarded but whose effect is kept) is the right abstraction for
error-as-exception checks: the `peek` runs, decides to throw, and the
result tuple is unaffected. This pattern generalises cleanly to a
`Throws` wrapper in our design.

**What we want to avoid.** Five things.

The DSL is a preprocessor on top of Haskell, not Haskell. No HLS, no
GHCi, no type-driven completion, no Haddock pulling through. Edward Yang's
"Call and fun: marshalling redux"
(http://blog.ezyang.com/2010/06/call-and-fun-marshalling-redux/) — linked
from the issue — catalogues the resulting pain.

The marshaller composition is positional and closed: the `&` modifier is
hard-coded to N=2; arbitrary N (a struct mapped to three C slots) is not
expressible. We want N to be open in our design.

The default-marshaller table is type-pair lookup, not extensible by the
user without the `{#default#}` directive. Even with the directive, you
cannot say "any newtype around a `Storable a` marshalls the same way as
`a`". A type-class-based design replaces table-lookup with instance
resolution, which is open-world.

The verbose generated code is unreadable. `cFloatConv . sinf'_ . cFloatConv`
is the kind of thing a user has to debug when something goes wrong.
Generated Haskell from our design should be readable Haskell, full stop.

Finally, c2hs has historically struggled with cross-compilation because
the tool needs to run a `sizeof`/`alignof` probe. `hs-bindgen` solves
this through libclang; the design here inherits that solution by virtue
of sitting *on top of* generated low-level bindings.

### 3.2 OCaml `ctypes` — values, not syntax

OCaml `ctypes` (Yallop et al., 2018) is the modern reference design for
"FFI binding as a library, not a preprocessor". A function description
is just an OCaml value:

```ocaml
let sigaddset =
  foreign "sigaddset" (ptr sigset_t @-> int @-> returning int)
```

The right-associative `@->` operator and `returning` combinator build up
a value of type `(sigset_t ptr -> int -> int) fn`. `foreign` consumes
that value and the C symbol name and produces an OCaml function. No
preprocessor.

The other contribution is the `view` combinator:

```ocaml
val view : read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ

let string =
  view ~read:string_of_char_ptr ~write:char_ptr_of_string (ptr char)
```

`view` lets you say: "represent the OCaml side as `string`, the C side
as `char*`, here is the conversion in each direction." Crucially, the
*function* described as `string @-> returning int` then automatically
applies the conversion at the boundary — every C `char*`-taking
function uses the same `string` view, and no user-side wrapper is
needed.

**What we want.** The bidirectional `(read, write)` pair is precisely
the L1 marshalling shape: one Haskell type maps to one C type, two
conversion functions glue them, every callsite uses the same conversion.
This is the prototypical instance of our `Marshal` class.

`view`'s purity (no IO bracket; OCaml's GC handles lifetime) is what
*differs* from Haskell, where we need explicit `withCString`-shaped
brackets. The Haskell analog of `view` is therefore a `Codensity IO`
pair, not a `(→, →)` pair. We discuss the consequence in §5.

**What we want to avoid.** The "function description as a value" idea is
elegant but the user's prompt explicitly asks for a type-class-based
design. We'd lose discoverability (it's hard to ask GHC "what types are
marshallable?") and lose the open-world extensibility that the type-class
approach gives us for free.

`ctypes` also does not natively solve the "1 OCaml value → N C args" case
— users describe the raw C function with N slots and then write an OCaml
wrapper that pre-processes the args. That's the gap our `Argument` /
`Result` classes are designed to close.

### 3.3 `inline-c` — `Context` as a monoid

`inline-c` (FPComplete) is an inline-C quasi-quoter:

```haskell
countBits :: BS.ByteString -> IO CInt
countBits bs = [C.block| int {
  int i, bits = 0;
  for (i = 0; i < $bs-len:bs; i++)
    bits += __builtin_popcount($bs-ptr:bs[i]);
  return bits;
} |]
```

The interesting abstraction is `Context`, a *monoid* of marshalling
competence. `bsCtx` provides `$bs-ptr` and `$bs-len` anti-quoters;
`vecCtx` provides `$vec-ptr` and `$vec-len`; users compose `baseCtx <>
bsCtx <> vecCtx <> funCtx` and add their own.

**What we want.** Two ideas:

- Marshalling competence is *open* — a domain library (one for `bytestring`,
  one for `vector`, one for the user's own newtypes) contributes its own
  marshallers in its own module. Translated to type classes: each
  package defines its own `Marshal`/`Argument` instances; no central
  registry.

- The "one Haskell value, multiple anti-quoter views" pattern (`$bs-ptr`
  and `$bs-len` over the same `bs`) is `inline-c`'s answer to multi-slot.
  In our class-based design, a single `Argument (PtrLen ByteString)`
  instance carries both views internally.

**What we want to avoid.** `inline-c` still requires the user to write
C source. It marshals at the call site, not at a single named-binding
boundary. If you call `strlen` from ten places, you write the marshalling
ten times. That is exactly the failure mode our typed wrappers address.

### 3.4 Two-tier libraries — `vulkan` et al.

For completeness: the Haskell ecosystem has a strong convention of
splitting bindings into two layers. The high layer typically has one
or two type-class pairs. `vulkan`:

```haskell
class ToCStruct   a where withCStruct  :: a -> (Ptr a -> IO b) -> IO b
class FromCStruct a where peekCStruct  :: Ptr a -> IO a
```

`OpenGL`:

```haskell
class HasGetter t a where get  :: t -> IO a
class HasSetter t a where ($=) :: t -> a -> IO ()
```

These are independent reinventions of the L1 marshalling pair. Each
library reinvents it because no canonical version ships in
`hs-bindgen-runtime`. That is the gap this proposal closes.

### 3.5 Theoretical lineage

For posterity. Two recent papers worth citing:

- **Yallop, Sheets, Madhavapeddy — "A Modular Foreign Function
  Interface"** (SCP 2018). The OCaml `ctypes` design as a paper.
  Establishes that an FFI binding description is parameterisable over a
  *backend* (libffi vs Cstubs vs mocked). Our analog is L3's `Call`
  class being interpretable by the PP backend, the TH backend, or a
  test mock.

- **Xia, Orchard, Wang — "Composing Bidirectional Programs Monadically"**
  (Haskell 2019). Biparsers (read + write) as profunctors. The L1
  `Marshal` class is a `Codensity IO` profunctor in disguise; the
  formalisation here covers it.

Both convergent. The shape we want is well-known.

---

## 4. Where `hs-bindgen` is today

A self-contained tour of the existing in-tree machinery that the
proposed design builds on. None of this is changing; the new layer
sits on top.

### 4.1 `HasFFIType` — newtype erasure

The class
(`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/HasFFIType.hs:103-114`):

```haskell
class HasFFIType a where
  type ToFFIType a :: FFI.FFIType
  toFFIType   :: a -> FFIType a
  fromFFIType :: FFIType a -> a
```

Its job is mechanical: strip newtypes off Haskell types so that
`foreign import` declarations can be expressed in the underlying C
types. It is *pure* (no IO) and *total* (no failures). Trivial example:

```haskell
deriving newtype instance HasFFIType CInt          -- CInt → CInt
deriving via ViaNewtype (Ptr a) instance HasFFIType (PtrConst a)
-- PtrConst a → Ptr a (via the underlying ConstPtr newtype)
```

The crucial fact for our purposes is the instance for function arrows
(`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/HasFFIType.hs:233-238`):

```haskell
instance (HasFFIType a, HasFFIType b) => HasFFIType (a -> b) where
  type ToFFIType (a -> b) = FFI.FunArrow (ToFFIType a) (ToFFIType b)
  toFFIType   f = \x -> toFFIType   (f $ fromFFIType x)
  fromFFIType f = \x -> fromFFIType (f $ toFFIType   x)
```

That is exactly `dimap fromFFIType toFFIType`. So
`HasFFIType` already *is* a profunctor for the purpose of erasing
newtypes from function signatures.

What `HasFFIType` does *not* do — and what blocks us from using it as
the high-level marshalling layer — is anything that requires `IO`.
`withCString` is not a pure conversion; it's a bracket. `peekCString` is
in `IO`. So `HasFFIType` can take us from `Bool` to `CBool` via a pure
`Num`-style coercion, but not from `String` to `Ptr CChar`.

### 4.2 `ToFunPtr` / `FromFunPtr` — the model for L1

The closest existing in-tree precedent for the L1 marshalling class is
the callback class pair
(`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/FunPtr/Class.hs:23-41`):

```haskell
class ToFunPtr   a where toFunPtr   :: a -> IO (FunPtr a)
class FromFunPtr a where fromFunPtr :: FunPtr a -> a

withFunPtr :: ToFunPtr a => a -> (FunPtr a -> IO b) -> IO b
withFunPtr x = bracket (toFunPtr x) freeHaskellFunPtr
```

This pair is L1 for one specific use case (callbacks). `toFunPtr` /
`fromFunPtr` are the "write" / "read" pair; `withFunPtr` is the
bracketed form derived from them. The convention is clean: a single
type-class pair per direction, plus a `withX` derived bracket.

The proposed design *generalises* this exact shape. `Marshal` is the
class pair; `withMarshal` is the bracket; the deriving-via and
instance-resolution discipline carries over verbatim.

### 4.3 The supporting cast

A few other in-tree classes deserve mention as building blocks:

- **`StaticSize` / `ReadRaw` / `WriteRaw`**
  (`hs-bindgen-runtime/src/HsBindgen/Runtime/Marshal.hs`) — generalised
  `Storable`. Many bindings derive `Storable` via `EquivStorable` from
  these three. We need them for `Result` instances that allocate scratch
  buffers and `peek` them.

- **`IsArray`** (`HsBindgen.Runtime.IsArray`) — a single-method class
  `withElemPtr :: a -> (Ptr (Elem a) -> IO r) -> IO r` that lets
  `IncompleteArray` (and other array-like types) be used as pointer
  sources for FFI calls. This is exactly the L1 shape for arrays.

- **`HasCField`** (`HsBindgen.Runtime.HasCField`) — `peek` / `poke` on
  named struct fields. Orthogonal to this proposal; we use struct field
  access as-is.

- **`CEnum`** (`HsBindgen.Runtime.CEnum`) — `toCEnum` / `fromCEnum` are
  the L1 conversion for enums. Already covers §2.1 of the patterns.

### 4.4 Map to the state of the art

| State of the art | Existing `hs-bindgen-runtime` analog | Gap |
|------------------|--------------------------------------|-----|
| c2hs default pure marshallers (`fromBool`, `fromIntegral`, …) | `HasFFIType`, `CEnum` | None |
| c2hs `with*` IO bracket marshallers | `ToFunPtr` / `withFunPtr` (specialised) | **A general `Marshal` class missing** |
| c2hs `&` (one Hs to two C) | Not encoded; each binding writes it inline | **`Argument` class missing** |
| c2hs `alloca-` out-parameter | Not encoded; users use `alloca` directly | **`Result` class missing** |
| OCaml ctypes `view` | `HasFFIType` (pure direction only) | **IO-flavoured version missing** |
| OCaml ctypes `foreign` | `foreign import` directly | None — we don't need value-level descriptors |
| inline-c `Context` monoid | Not applicable (class-based instead) | Open-world type classes give the same property |
| `vulkan`'s `ToCStruct` / `FromCStruct` | One-off per binding | **A single canonical class missing** |

The summary in one line: **`hs-bindgen-runtime` already has all the
*support* types (`Ptr`, `PtrConst`, `IncompleteArray`, `CEnum`, …); what
is missing is the *vocabulary* — the class pair that says "this Haskell
type marshals to that C representation, here is how, in both
directions".**

---

## 5. Suggested design

A three-layer class hierarchy, built incrementally. We motivate each
layer by showing what the worked examples in §2 demand of it.

### 5.1 L1 `Marshal` — one Haskell value ↔ one C value

The starting point is the bracket-style bidirectional pair. From §2.1:
`String` needs to become `PtrConst CChar` for the duration of the
`strlen` call. The shape that makes that work, modeled on the existing
`ToFunPtr` / `FromFunPtr`:

```haskell
class Marshal hi where
  type Foreign hi :: Type
  withMarshal :: hi -> (Foreign hi -> IO r) -> IO r
  peekMarshal :: Foreign hi -> IO hi
```

`Foreign hi` is the C-side representation; `withMarshal` is the
bracket; `peekMarshal` is the inverse (used by results). The instance
for `String`:

```haskell
instance Marshal String where
  type Foreign String = PtrConst CChar
  withMarshal s k     = withCString s (k . PtrConst.unsafeFromPtr)
  peekMarshal p       = peekCString (PtrConst.unsafeToPtr p)
```

For *pure* conversions, we don't want users to write `\x k -> k (...)`.
We provide a deriving-via newtype `Pure`:

```haskell
newtype Pure a = Pure a
instance HasFFIType a => Marshal (Pure a) where
  type Foreign (Pure a) = FFIType a
  withMarshal (Pure x) k = k (toFFIType x)
  peekMarshal y          = pure (Pure (fromFFIType y))
```

So all of `CInt`, `CBool`, `CSize`, and any newtype with `HasFFIType`
get an instance via:

```haskell
deriving via Pure CInt instance Marshal CInt
```

And the project's existing `CEnum` types
(`Qrcodegen_Ecc`, `Qrcodegen_Mask`, etc.) inherit `Marshal` for free
because they already derive `HasFFIType`.

**Folding nullability into `Marshal`.** §2 of the patterns has `NULL ↔
Maybe a`. The original sketch in `ISSUE_ANALYSIS.md` made this a separate
`MarshalNullable` class, on the grounds that "we cannot claim `Marshal
(Maybe Bool)`". The team's preference (open question Q1) is to keep
everything in one class. The cleanest way to thread that needle is to
let the `Maybe` instance carry the *constraint* directly:

```haskell
instance (Marshal a, Foreign a ~ Ptr p) => Marshal (Maybe a) where
  type Foreign (Maybe a) = Foreign a
  withMarshal Nothing  k = k nullPtr
  withMarshal (Just x) k = withMarshal x k
  peekMarshal p
    | p == nullPtr = pure Nothing
    | otherwise    = Just <$> peekMarshal p
```

The constraint `Foreign a ~ Ptr p` makes `Marshal (Maybe Bool)`
ill-typed at instance-resolution time (since `Foreign Bool = CBool ≠ Ptr
_`), which is exactly the discipline we want. If we ever need to
support `PtrConst`-shaped or `FunPtr`-shaped nullables, we introduce a
small auxiliary class `IsPointerShaped p` with `nullValue` and `isNull`
methods and instances for `Ptr`, `PtrConst`, `FunPtr`. Either way,
nullability never escapes `Marshal`.

This addresses Q1: `Marshal` stays single-class, no separate
`MarshalNullable`.

### 5.2 L2 `Argument` / `Result` — multi-slot

The §2.2 example demands more: `do_thing(str, len)` has *one* Haskell
argument feeding *two* C slots. L1's `Foreign hi :: Type` is
fundamentally single-slot. We need a tier where a Haskell value
corresponds to a *list* of C slots.

#### 5.2.1 The slot-list representation choice

The team raised open question Q5: do we use `HList`-style for the slot
list, or can we get away with an N-ary curried representation? Both are
viable. Below is the comparison.

**Option A — HList**:

```haskell
class Argument hi where
  type ArgC hi :: [Type]
  withArg :: hi -> (HList (ArgC hi) -> IO r) -> IO r
```

So `ArgC (PtrLen ByteString) = '[Ptr CChar, CSize]`. The continuation
takes a single `HList` parameter; the `Call` class type-family-walks
the list and feeds each element to the low-level function.

Pro: composition is trivial (`Concat`). Single uniform parameter shape.

Con: users see `HList` in error messages. Concatenating type-level lists
requires `OVERLAPPING` instances or a closed type family. We'd need a
small `HList` (~30 LOC), or use `Data.Vec.Lazy` already in the project's
dep tree (`hs-bindgen/src-internal/HsBindgen/BindingSpec/Gen.hs:17`).

**Option B — N-ary curried continuation** (preferred per Q5):

```haskell
class Argument hi where
  type ArgF hi (r :: Type) :: Type
  withArg :: hi -> ArgF hi (IO r) -> IO r
```

`ArgF hi r` is a type-level "shape" that yields the curried function
type from the C slot list to `r`:

```haskell
-- For a single-slot argument:
type instance ArgF (Single hi) r = Foreign hi -> r

-- For PtrLen ByteString:
type instance ArgF (PtrLen ByteString) r = Ptr CChar -> CSize -> r
```

So `withArg :: PtrLen ByteString -> (Ptr CChar -> CSize -> IO r) -> IO r`.
The continuation is just an ordinary curried function — no `HList` at
the call site, no `(::: HNil)` tail. Concrete instance:

```haskell
newtype PtrLen a = PtrLen a

instance Argument (PtrLen ByteString) where
  type ArgF (PtrLen ByteString) r = Ptr CChar -> CSize -> r
  withArg (PtrLen bs) k =
    BS.useAsCStringLen bs $ \(p, n) -> k p (fromIntegral n)
```

Pro: no `HList`. Error messages and Haddock show the curried function
shape, which Haskell programmers already understand. The low-level
function signature is itself N-ary, so the continuation shape matches
naturally.

Con: composition is slightly more involved — the `Call` class instance
that walks the high-level signature has to chain `ArgF` applications.
We sketch the elaboration in §5.4 and show it works for the patterns
we care about.

**Recommendation**: take Option B. The user-facing API is cleaner, and
the type-family machinery for `Call` is no worse than what `servant`
already deploys for routes. If during implementation the `Call`
elaboration turns out to be intractable for some pattern we missed, we
fall back to Option A; the user-visible classes can be kept the same
shape (just changing the type family from "N-ary curry" to "HList") if
that happens.

This addresses Q5: N-ary continuations preferred; HList retained as a
fallback.

#### 5.2.2 The `Argument` class

```haskell
class Argument hi where
  type ArgF hi (r :: Type) :: Type
  withArg :: hi -> ArgF hi (IO r) -> IO r
```

The default-style instance for the single-slot case piggybacks on
`Marshal`:

```haskell
newtype Single hi = Single hi
instance Marshal hi => Argument (Single hi) where
  type ArgF (Single hi) r = Foreign hi -> r
  withArg (Single x) k = withMarshal x k
```

And the multi-slot instances are written by hand, one per pattern:

```haskell
instance Argument (PtrLen ByteString) where
  type ArgF (PtrLen ByteString) r = Ptr CChar -> CSize -> r
  withArg (PtrLen bs) k =
    BS.useAsCStringLen bs $ \(p, n) -> k p (fromIntegral n)

instance Storable a => Argument (PtrLen (IncompleteArray a)) where
  type ArgF (PtrLen (IncompleteArray a)) r = Ptr a -> CSize -> r
  withArg (PtrLen arr) k =
    IsA.withElemPtr arr $ \p ->
      k p (fromIntegral (VS.length (IA.toVector arr)))
```

Three things to note. First, multi-slot is *opt-in* — users wrap a
type in `PtrLen` (or `Out`, etc.) to signal "treat this differently".
This protects the simple cases from the multi-slot machinery and makes
intent explicit at the wrapper signature.

Second, the `PtrLen` newtype works for any Haskell array-shaped value;
the user adds new instances in their own module without touching the
library. This is the inline-c `Context`-monoid lesson, encoded as type
classes.

Third, the open-question discussion in `ISSUE_ANALYSIS.md` §6.3 about
folding `Argument` into `Marshal` via default methods (one class
instead of three) is still on the table; we treat it as an
implementation-time choice. The user-visible API of `hl` is the same
either way.

#### 5.2.3 The `Result` class

The mirror image for return values:

```haskell
class Result hi where
  type ResScratch hi :: [Type]    -- slots we pre-allocate for outputs
  type ResRet     hi :: Type      -- C-side return value type
  peekRes :: HList (ResScratch hi) -> ResRet hi -> IO hi
```

(Result *does* use a small `HList` here even with Option B for
`Argument`, because the natural shape of scratch is a list, not a
function. We could use a nested tuple instead — `()`, `(a,)`, `(a, b)`,
etc. — if even the small `HList` is undesirable. The trade-off is
minor and can be revisited.)

The single-return case is the default:

```haskell
newtype SingleRes hi = SingleRes hi
instance Marshal hi => Result (SingleRes hi) where
  type ResScratch (SingleRes hi) = '[]
  type ResRet     (SingleRes hi) = Foreign hi
  peekRes HNil y = SingleRes <$> peekMarshal y
```

The out-parameter case (§2.3 — `get_size`):

```haskell
newtype Out hi = Out hi
instance (Marshal hi, StaticSize (Foreign hi), Storable (Foreign hi))
      => Result (Out hi) where
  type ResScratch (Out hi) = '[Ptr (Foreign hi)]
  type ResRet     (Out hi) = CInt
  peekRes (resPtr ::: HNil) _status =
    Out <$> (peekMarshal =<< peek resPtr)
```

The buffer-plus-length case (§2.4 — `srp6_server_session_step1`):

```haskell
data Buf a = Buf { bufCap :: CSize, bufResult :: IncompleteArray a }

instance Storable a => Result (Buf a) where
  type ResScratch (Buf a) = '[Ptr a, Ptr CSize]
  type ResRet     (Buf a) = CInt
  peekRes (bufP ::: lenP ::: HNil) _status = do
    actualLen <- peek lenP
    arr <- IA.peekArray (fromIntegral actualLen) (IA.toPtr bufP)
    pure (Buf actualLen arr)
```

**Renaming the multi-result case.** Q4 flagged that `Multi (Int, Int)`
doesn't read as "output". The team's preference is to use a name that
clearly signals direction. The proposed renames:

- `Out`     — single out-parameter (kept)
- `Outs`    — multi out-parameter, e.g. `Outs (Int, Int)` (replaces `Multi`)
- `Buf`     — buffer-plus-length output (kept)
- `Throws e` — wraps any `Result` to translate a non-zero `CInt` into
  exception `e` (kept; matches the c2hs `*-` idiom)

`Outs (Int, Int)` reads as "two out-parameter results" naturally. We
adopt this rename.

This addresses Q4: rename `Multi` → `Outs`.

### 5.3 The L3 combinator `hl`

The user-facing API is a single overloaded function. From §2.1's example:

```haskell
hsStrlen :: String -> IO Int
hsStrlen = hl strlen
```

GHC has to elaborate `hl` so that the high-level signature `String -> IO
Int` corresponds, slot-by-slot, to the low-level signature `PtrConst
CChar -> IO CSize`. The class:

```haskell
class Call hi lo where
  hl :: lo -> hi
```

Argument step (one argument from the high-level signature consumes the
matching slots in the low-level signature):

```haskell
instance ( Argument a
         , Call rest lo'
         , lo ~ ArgF a lo'
         )
      => Call (a -> rest) lo where
  hl f a = withArg a $ \rest_args -> hl (rest_args :: lo') f_applied
    -- pseudocode; actual elaboration uses applyArgF :: ArgF a lo' -> lo
```

Result step (we hit `IO r` and the corresponding low-level shape is
`<scratch slots> -> IO (ResRet r)`):

```haskell
instance ( Result r
         , Scratch (ResScratch r)
         , lo ~ ApplyScratch (ResScratch r) (IO (ResRet r))
         )
      => Call (IO r) lo where
  hl f =
    withScratch $ \scratch_args ->
      peekRes scratch_args =<< applyScratch scratch_args f
```

`Scratch` is a tiny helper class that allocates each `ResScratch` slot:

```haskell
class Scratch (xs :: [Type]) where
  withScratch :: (HList xs -> IO r) -> IO r

instance Scratch '[] where
  withScratch k = k HNil

instance (Scratch xs, StaticSize a, WriteRaw a)
      => Scratch (Ptr a ': xs) where
  withScratch k =
    Marshal.with @a undefined $ \p ->
    withScratch              $ \ys ->
    k (p ::: ys)
```

The elaboration is the same shape as `servant`'s `HasServer` — two
instance heads on `Call`, one for arrows, one for the base IO type — but
on a simpler surface (no route DSL, no method dispatch). We expect this
to compile; we expect the error messages without `TypeError` polish to
be ugly. See §6.

### 5.4 Module organisation

Per Q6, the modules live under a new `HighLevel` namespace inside
`hs-bindgen-runtime`:

```
hs-bindgen-runtime/src/HsBindgen/Runtime/HighLevel/
    Marshal.hs          -- Marshal class, Pure, Maybe instance, primitive instances
    Argument.hs         -- Argument class, Single, PtrLen, …
    Result.hs           -- Result class, SingleRes, Out, Outs, Buf, Throws, Scratch
    Call.hs             -- Call class, hl
    Prelude.hs          -- re-exports for convenient import
```

Convention matches the existing `HsBindgen.Runtime.*` shape. Users
import qualified:

```haskell
import HsBindgen.Runtime.HighLevel.Prelude qualified as HL
hsStrlen :: String -> IO Int
hsStrlen = HL.hl strlen
```

This addresses Q6: namespace under `HsBindgen.Runtime.HighLevel.*`.

### 5.5 Where things slot in

Putting the §2 examples through the full design:

```haskell
-- §2.1
hsStrlen :: String -> IO Int
hsStrlen = hl strlen
  -- Marshal String         (library)
  -- Marshal Int            (library, via Pure)
  -- Call (String -> IO Int) (PtrConst CChar -> IO CSize)

-- §2.2
hsDoThing :: PtrLen ByteString -> IO Int
hsDoThing = hl do_thing
  -- Argument (PtrLen ByteString)  (library)
  -- Marshal Int                   (library)

-- §2.3
hsGetSize :: IO (Out Int)
hsGetSize = hl get_size
  -- Result (Out Int)              (library)

-- §2.4 — the big one
srp6ServerSessionStep1
  :: ServerSession -> PtrLen Verifier -> GroupId -> HashId -> RNG -> CSize
  -> IO (Buf Word8)
srp6ServerSessionStep1 = hl botan_srp6_server_session_step1
  -- five domain-newtype Marshal instances (user-written via Pure)
  -- one Argument (PtrLen Verifier)        (newtype-derived from library)
  -- Result (Buf Word8)                    (library)
```

Every instance the user has to write is either trivial (`deriving via
Pure ...`) or has a clear library precedent (a `PtrLen` instance
specialised to their domain type). The five lines of `deriving` plus
one `Argument` instance replace 27 lines of mechanical glue.

### 5.6 Use from the PP backend and TH backend

Per Q8: the design must be usable by both the Preprocess (PP) backend
(which emits standalone `.hs` files at build time) and the TH backend
(which generates declarations at compile time via splices).

The classes themselves are ordinary Haskell — they emit into module
declarations, can be referenced from generated TH `Dec`s, and have no
preprocessor or build-time dependency. Either backend can:

- Emit `instance Marshal MyType where ...` declarations for generated
  newtype wrappers (the PP backend writes this into the `.hs` file; the
  TH backend produces a corresponding `InstanceD`).
- Reference `hl` in generated wrappers via fully-qualified names
  (`HsBindgen.Runtime.HighLevel.Call.hl`), with no special build dance.

For now, *neither* backend will emit `hl` calls — Milestone 2.5 is
library-only, so the user manually writes the `hl` line. Milestone 3
generation is the natural next step and is supported by the design.

Note: the "quickcheck-style mock" backend mentioned in earlier drafts
was a stray theoretical reference — it referred to a testing backend
that intercepts the C call with arbitrary values instead of actually
calling C, useful for property-testing the high-level wrapper. It is
not load-bearing and not part of the proposal; only PP and TH backends
are needed.

This addresses Q8.

---

## 6. Caveats

Where the design might not be a good idea, in priority order.

### 6.1 The `Call` class will produce bad error messages by default

The risk that the Phase 3 evaluator flagged as the single biggest. The
elaboration mechanism is morally identical to `servant`'s `HasServer`:
a type class with two instance heads (arrow case and base case) walks
the user's high-level signature and unifies it against the low-level
signature via a string of type families (`ArgF`, `ApplyScratch`, …).
When the user misspells a type, omits an instance, or has an arity
mismatch, GHC's default error message will be a multi-screen wall of
"could not match `ArgF (PtrLen ByteString) lo0` with `Ptr CChar ->
CSize -> lo0`" failures.

**Mitigation**, deferred per Q7 but worth recording for when we get
there:
- Add `TypeError`-clad instance heads that detect the common failure
  modes (missing `Marshal Foo` → "Add `deriving Marshal via Pure Foo`
  or write an instance manually"; arity mismatch → "Expected N
  arguments, got M").
- Add a smoke test that deliberately triggers each common error and
  freezes the error text via golden tests, so regressions are caught.

If the polish is skimped, the design will earn `servant`'s reputation
unfairly. We need budget for it in the implementation phase.

### 6.2 The `Argument` and `Result` machinery may be over-engineered

`ISSUE_ANALYSIS.md` §2 catalogues four multi-slot patterns. If a real
binding survey shows that *only* `PtrLen` matters in practice — that
`Out`, `Buf`, `Outs` are rare enough to write inline at call sites —
then the L2 class hierarchy is paying its weight only for one pattern,
and a single hand-written `instance Marshal (PtrLen a)` ad-hoc would
be simpler.

**Mitigation:** before merging, port two real bindings (Botan,
c-qrcode) and count how many `Out` / `Buf` instances each needs. If
the count is "one each and we're done", reconsider whether `Result`
needs to be a class or can be a couple of utility functions.

### 6.3 The N-ary `ArgF` elaboration may not survive contact with GHC

Option B (curried continuations) is more ergonomic but the type-family
chaining at the `Call` class is finicky. There are known edge cases
where GHC's type-family reduction order interacts badly with
fundep-style elaboration. If we hit one, we fall back to Option A
(`HList` slot lists) — the user-visible API stays the same but error
messages get an extra layer of `HList`-ness.

**Mitigation:** in the implementation, write the `Call` elaboration
*twice* (once with `ArgF`, once with `HList`) and pick whichever
elaborates more cleanly. The library code is small enough that this
isn't expensive.

### 6.4 Performance pessimisations are possible

The bracket-style API for L1 (`withMarshal :: a -> (b -> IO r) -> IO r`)
is `Codensity IO`, which composes cleanly but adds one closure
allocation per argument unless GHC inlines aggressively. We need
`INLINE` pragmas on `withMarshal` for primitive instances, the way
`HasFFIType` already does.

**Mitigation:** an `INLINE` pass through the canonical instances. The
performance baseline is the hand-written wrapper, which has the same
shape; we should not be slower.

### 6.5 We are picking a representation choice for "the C result"

The `Result` class has both `ResScratch` (pre-allocated out slots) and
`ResRet` (the C return value). This bakes in the assumption that
*every* C function returns a single value, possibly augmented by
out-parameters. C functions that return nothing (`void`) use
`ResRet = ()`, but the API forces them to pass through `IO ()`. This
is fine but not minimal; a more elegant design might have `Result`
take a list of return slots too. Whether that's worth the extra
generality is an open question — see §1.3.

### 6.6 The `Storable` / `ReadRaw`-`WriteRaw` divergence (Q3)

The team's question (Q3) was: aren't `Storable` and `ReadRaw`/`WriteRaw`
the same thing? Almost. The runtime defines `StaticSize`, `ReadRaw`,
and `WriteRaw` as more general / less constrained replacements for
`Storable`'s methods; `EquivStorable` (`hs-bindgen-runtime/src/HsBindgen/Runtime/Marshal.hs`)
provides a deriving-via newtype that turns a type with the three
project classes into a `Storable` instance.

The practical upshot is: any type the user defines and wants to use
through this library should derive `Storable` via `EquivStorable`. Once
that holds, `Result (Out hi)` etc. work without further plumbing. The
"divergence" the original draft worried about is a non-issue *for the
proposed design* — it would only matter if we needed to peek/poke
inside a type that has `ReadRaw`/`WriteRaw` but not `Storable`, which
is not a case we generate.

This addresses Q3 — the answer is "no divergence to worry about in
practice".

### 6.7 We deliberately exclude exception semantics from the default

A C function returning `-1 on error` is the single most common
convention in C APIs. The proposal does not bake any
exception-throwing into `Marshal` or `Result` defaults; users opt in
via a `Throws ErrorType` wrapper around their `Result`. This is a
deliberate choice (Q2) — it keeps the default behaviour transparent
and matches the c2hs lesson that aggressive defaults mislead.

**Risk**: users will be tempted to write `Throws` everywhere by hand,
duplicating the wrapping. Mitigation: provide a small `throwsResultIO`
convenience, plus a manual page chapter "writing error-handling
wrappers". Not load-bearing.

This addresses Q2 — kept as an explicit non-feature.

---

## Appendix: open questions tracker

The questions raised in `ISSUE_ANALYSIS.md` §7, with their status in
this proposal:

| Q | Topic | Resolution |
|---|-------|------------|
| Q1 | Nullability — separate class or in `Marshal`? | Folded into `Marshal` via a `Foreign a ~ Ptr _` constraint on the `Maybe` instance. §5.1 |
| Q2 | Default error-handling | Explicit non-feature; `Throws` wrapper is the opt-in mechanism. §6.7 |
| Q3 | `Storable` vs `ReadRaw`/`WriteRaw` divergence | Non-issue in practice; `EquivStorable` bridges. §6.6 |
| Q4 | Naming — `Multi` doesn't read as output | Renamed to `Outs`. §5.2.3 |
| Q5 | `HList` vs N-ary curried continuation | N-ary preferred (Option B); HList retained as fallback. §5.2.1 |
| Q6 | Module namespace | `HsBindgen.Runtime.HighLevel.*`. §5.4 |
| Q7 | `TypeError` polish | Kept open; load-bearing but deferable. §6.1 |
| Q8 | Backend modularity / "quickcheck-style mock" | Mock was a stray reference; design supports PP + TH backends. §5.6 |

[issue-8]: https://github.com/well-typed/hs-bindgen/issues/8
