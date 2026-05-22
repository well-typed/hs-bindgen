# Issue #8 — Compositional high-level bindings: design study

> ⚠️ **Status: archived workflow notes.** The canonical proposal is in
> [`HIGH_LEVEL_DESIGN.md`](./HIGH_LEVEL_DESIGN.md). This document
> retains the audit trail of the design process: prior art catalogue,
> plans considered, Phase 3 evaluator verdict, file:line verification,
> and the original open-question discussion. Use it to understand *how*
> the design landed where it did; use `HIGH_LEVEL_DESIGN.md` to read
> *what* the design actually is.
>
> Per the user's framing, this was a **research / design exploration**,
> not an implementation ticket.

## 1. Executive summary

`hs-bindgen` emits **low-level** bindings: signatures full of `Ptr CChar`,
`CInt`, `CBool`, `PtrConst`, `IncompleteArray Word8`, etc. The library ships
a runtime support package (`hs-bindgen-runtime`) with helpers like
`IncompleteArray`, `PtrConst`, `IsArray`, `CEnum`, `HasCField`, and
`HasFFIType` that already act as a low-level marshalling toolkit.

What is missing — and what the project roadmap explicitly calls out as
**Milestone 2.5** ("library support for hand-written high-level bindings"
in `manual/roadmap.md:76-83`) — is a library of *patterns* that lets a user
lift a generated low-level signature into an ergonomic high-level one with
the smallest possible amount of glue. Concretely, today a user writes

```haskell
basicDemo :: IO ()
basicDemo = do
  F.withCAString "Hello, world!" $ \text ->
    F.allocaArray ... $ \tempBuffer -> do
      F.allocaArray ... $ \qrCode -> do
        b <- QR.qrcodegen_encodeText
              (PtrConst.unsafeFromPtr text) tempBuffer qrCode
              QR.Qrcodegen_Ecc_LOW QR.qrcodegen_VERSION_MIN
              QR.qrcodegen_VERSION_MAX QR.Qrcodegen_Mask_AUTO
              (F.fromBool True)
        ...
```
(`examples/c-qrcode/hs-project/app/Main.hs:66-75`)

and we would like them to be able to write something like

```haskell
qrEncodeText :: String -> Ecc -> Version -> Version -> Mask -> Bool
             -> IO (Maybe QRCode)
qrEncodeText = hl wrap_qrcodegen_encodeText
```

with all of the `withCString`/`allocaArray`/`fromBool`/`unsafeFromPtr`
plumbing supplied by a tiny set of typeclass-driven combinators.

### TL;DR of the proposal

A **three-layer class hierarchy** sitting above the existing
`HasFFIType` / `ReadRaw` / `WriteRaw` infrastructure:

| Layer | Purpose | Algebraic structure |
|------:|---------|---------------------|
| **L0** — `HasFFIType` *(exists)* | Erase newtypes to raw foreign types | profunctor `(b → a, a → b)` |
| **L1** — `Marshal` | Bidirectional bracket-style conversion: one Hs value ↔ one C value (`String ↔ Ptr CChar`, `Bool ↔ CBool`, …) | Codensity-IO profunctor |
| **L2** — `Argument` / `Result` | Multi-slot: one Hs value ↔ **multiple** C slots (the c2hs `&` case, out parameters, buffer+length, …) | free applicative over L1 |
| **L3** — `Call` | Glue an `Argument` chain and a `Result` chain to a concrete low-level binding | derived |

`L1` is closely modelled on the existing in-tree `ToFunPtr`/`FromFunPtr`
pair (`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/FunPtr/Class.hs:23-41`),
which is the canonical example: bidirectional, bracket-based, deriving-via
friendly. `L2` is what makes the user's "one Hs arg consumes two C slots"
case (the c2hs `&` modifier; `withCStringLen`-shaped marshallers) directly
expressible. `L3` is one generic combinator `hl :: lo -> hi` driven by a
type-class that walks both chains.

The design is **opt-in and layered**: simple cases use only L1; complex
cases drop to L2; and the user can always abandon the framework and write
the wrapper by hand. Nothing forces auto-generation; Milestone 3
(generation) can sit on top of these classes later if desired.

The rest of this document justifies the layering, surveys the prior art
that informed it, catalogues the concrete patterns that must be covered,
and walks through worked examples on the project's own `Botan` and
`c-qrcode` examples.

---

## 2. Concrete patterns we need to capture

This catalog combines:

- the marshalling-spec language of `c2hs` (User Guide; Chakravarty 1999
  paper; the parser-generator source at `src/C2HS/CHS.hs` and
  `src/C2HS/Gen/Bind.hs`);
- the high-level wrappers in the project's own `examples/` directory
  (`Botan.hs`, `QRCodeGenerator/Main.hs`, `libpcap/Pcap.hs`);
- the patterns described in the Well-Typed "Purgatory" blog post on
  Rust-through-C (2023);
- the `view` and `@->` combinators of OCaml `ctypes`;
- the `Context`/anti-quoter mechanism of `inline-c`.

Each pattern below is named, defined, and tagged with the tier on which it
naturally lives — **L1** (single Hs ↔ single C slot), **L2** (single Hs ↔
multiple C slots, possibly in/out), **L3** (whole-call concerns).

### 2.1 Pure conversions (L1)

A value-level `(Hs → C, C → Hs)` pair, no IO.

| Hs type | C type | Conversion | c2hs default |
|---------|--------|------------|--------------|
| `Bool` | integral C (`CBool`, `CInt`, …) | `fromBool` / `toBool` | yes |
| `Int` etc. | integral C | `fromIntegral` (a.k.a. `cIntConv`) | yes |
| `Char` | `CChar` | `castCharToCChar` / `castCCharToChar` | yes |
| `Float` | `CFloat` | `realToFrac` (a.k.a. `cFloatConv`) | yes |
| sum type | enum integer | `fromCEnum` / `toCEnum` | yes (via `{#enum#}`) |
| `Day`, `UTCTime`, etc. | `CTime` | bespoke pair | no |

c2hs encodes these as the "pure marshaller" case (no `*` modifier). Today
in `hs-bindgen` they are written ad-hoc as `F.fromBool`, `fromIntegral`,
etc. The generated `CEnum` class already gives us `fromCEnum`/`toCEnum`,
which is exactly the pure-conversion shape.

### 2.2 Bracket marshalling (L1)

A scoped (`Codensity IO`) conversion: allocate on the way in, free on the
way out. The canonical examples are

```haskell
withCString    :: String     -> (Ptr CChar -> IO a) -> IO a
with           :: Storable a => a -> (Ptr a   -> IO b) -> IO b
withArray      :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withFunPtr     :: ToFunPtr a => a -> (FunPtr a -> IO b) -> IO b
```

The last one is already in `hs-bindgen-runtime`. In c2hs the default
in-marshaller for `String`, `Storable a => a`, and `ForeignPtr a` is
exactly this shape, identified by the trailing `*` modifier in the spec
(`withCString*`, `with*`, `withForeignPtr*`).

### 2.3 Pointer + length pair (L2: 1 Hs → 2 C)

```haskell
withCStringLen   :: String       -> ((Ptr CChar, Int)   -> IO a) -> IO a
useAsCStringLen  :: ByteString   -> (CStringLen        -> IO a) -> IO a
withVector       :: V.Storable a => V.Vector a -> ((Ptr a, CSize) -> IO b) -> IO b
```

c2hs writes this with the `&` modifier in the type spec:
``withCStringLen* `String' & `` means "one Haskell `String` parameter, but
it contributes two consecutive C slots". This is the pattern the user's
prompt explicitly calls out:

```haskell
lowPutStrLn  :: Ptr CChar -> CInt -> IO ()
highPutStrLn :: String -> IO ()    -- one Hs arg → two C slots
```

The project already encounters this in `Botan.hs`:

```haskell
IsA.withElemPtr salt $ \saltPtr ->
let saltLen = fromIntegral $ VS.length $ IA.toVector salt in
   ...                                    -- saltPtr and saltLen passed to C
```

(`examples/botan/hs-project/src/Botan.hs:170-171`). Every bracket-with-
length call manually computes both halves and threads both into the call.

The Well-Typed "Purgatory" blog post identifies *exactly this pattern* as
the load-bearing convention of their Rust-through-C bindings: pointer +
length for variable-sized values, paired with a per-binding `withBorshVarBuffer`
helper.

### 2.4 Out parameter (L2: 0 Hs → 1 C, contributes a result)

```haskell
alloca $ \resPtr -> do
  c_get_size groupIdPtr resPtr
  peek resPtr
```

A C function returns a value through a caller-allocated pointer. In c2hs
this is `alloca- ... peek*` (the leading minus suppresses the argument
from the Haskell signature; the trailing `*` is a monadic `peek` whose
result becomes a component of the Haskell return tuple).

`examples/botan/hs-project/src/Botan.hs:237-243` (`srp6GroupSize`) is
this case verbatim.

### 2.5 In/out length parameter (L2: 1 Hs ↔ 2 C, both directions)

The classic C buffer convention: caller writes the buffer's capacity to a
`size_t*` before the call and reads back the actual size after.

```c
size_t cap = MAX;
status = c_call(.., out_buf, &cap);
// cap is now the actual length written
```

`Botan.hs:124, 149, 177` shows this. It is both an input (we poke `MAX` in)
and an output (we peek the actual count out). c2hs handles this awkwardly
with separately-declared in and out marshallers per parm.

### 2.6 Buffer + size returned together (L2/L3: 0 Hs → 2 C, contributes a result)

The C function takes `(buf_out :: Ptr Word8, len_out :: size_t*)` and fills
both. The caller's job is to reconstruct an `IncompleteArray Word8` or
`ByteString` from the two outputs. `Botan.hs:122-136` shows the full
dance. This is the dual of §2.3 on the result side.

### 2.7 Resource handles with init/destroy (L3 lifecycle)

```haskell
withRng :: RNGType -> (RNG -> IO a) -> IO a
withRng t = bracket (rngInit t) rngDestroy
```

Every C library that exposes opaque handles wants this. It's a `bracket`
that pairs an `init` with a `destroy`. `Botan.hs:72-86, 251-277` shows the
SRP6-session and RNG handles. c2hs has no first-class support; the user
writes `bracket` manually. *c2hs's `{#pointer ... foreign finalizer fin #}`
hook covers the *garbage-collected* version where the finalizer runs at
GC time, but the explicit-`bracket` lifecycle is left to the user.*

### 2.8 Return-code as exception (L3)

C convention: `int return == 0` is success, negative is failure. The
Haskell side wants either `IO ()` (success) or an exception.

`Botan.hs:61-64` defines a one-off `throwErrnoIfNegative`. c2hs's
`Foreign.C.Error` ships `throwErrno`, `throwErrnoIf`, `throwErrnoIfNull`;
they are used at call sites rather than encoded in the binding. The c2hs
`*-` modifier ("effectful out marshaller whose value is discarded") is
the dedicated machinery: `peekErrno*-` runs the check and threads nothing
into the result tuple.

### 2.9 Optional pointer (`NULL` ↔ `Maybe`) (L1)

Probably the single most common high-level transformation: a `Ptr a` that
may be null becomes `Maybe a`. c2hs has no direct support;
`Foreign.Marshal.Utils` ships `maybeNew`, `maybePeek`, `maybeWith`.

### 2.10 Variadic args (out of scope)

c2hs supports `{#fun variadic#}` for fixed-arity variadic calls.
`hs-bindgen` explicitly skips variadics today
(`manual/low-level/translation/functions.md:538-562`). I exclude this from
the proposal.

### 2.11 Callbacks (FunPtr ↔ Haskell function) (L1, already solved)

This is the one place where `hs-bindgen-runtime` already has a clean
type-class pair: `ToFunPtr` / `FromFunPtr` with the bracket helper
`withFunPtr`
(`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/FunPtr/Class.hs:23-41`).
I treat this as the canonical model for the rest of the design.

### 2.12 Struct field accessors (L1, already solved)

`HasCField` lets us read/write a named field on a `Ptr Struct`
(`hs-bindgen-runtime/src/HsBindgen/Runtime/HasCField.hs:50-126`). The
`OverloadedRecordDot` interaction makes this ergonomic. Not changed by
this proposal; we use it as-is.

### 2.13 Pattern priority summary

The patterns that **must** be covered by the new API:

1. Pure conversion (`Bool ↔ CBool`, `Int ↔ CInt`, sum-type ↔ enum) — §2.1
2. Bracket marshalling (`String ↔ Ptr CChar`) — §2.2
3. Pointer + length (`ByteString ↔ (Ptr CChar, CSize)`) — §2.3
4. Out parameter (`alloca`/`peek`) — §2.4
5. NULL ↔ Maybe — §2.9
6. Init/destroy as `bracket` — §2.7
7. Buffer + size returned — §2.6

Patterns we should **support but not force**:

8. In/out length — §2.5 (covered by L2 composition)
9. Result code → exception — §2.8 (a `Result` instance choice, not default)

---

## 3. Prior art

The following section is informed by two background research reports that
ran in parallel during this study: a c2hs-focused audit (sources include
the c2hs source tree `src/C2HS/CHS.hs`, `src/C2HS/Gen/Bind.hs`, the
`doc/c2hs.xml` user guide, and the ChangeLog) and a wider FFI-ecosystem
survey (sources: `hsc2hs`, `inline-c`, `bindings-DSL`, `vulkan`,
`OpenGLRaw`, `bytestring`, OCaml `ctypes`, Python `cffi`/`ctypes`, plus
theoretical references in the bidirectional-programming literature).

### 3.1 `hsc2hs`

The original tool. CPP-style pre-processing of `.hsc` into `.hs`: `#peek`,
`#poke`, `#size`, `#alignment`, `#const`. It does **no marshalling at
all**: the programmer writes the `Storable` instance and every
`withCString` by hand. `hs-bindgen` already covers the same ground more
typefully via `StaticSize` / `ReadRaw` / `WriteRaw` / `HasCField` plus
generated instances. **Take-away:** nothing positive; it's the "floor" of
what a tool can do.

### 3.2 `c2hs`

The closest thing to what we want, and the inspiration for issue #8. The
central hook is `{#fun#}` with grammar

```
{# fun [pure] [unsafe] [interruptible] cid [as (hsid | ^)]
   [verbhs =>] { parm_1, ..., parm_n } -> parm_result #}

parm  -> '+'
      | [inmarsh [* | -]] ['%'] verbhs ['&'] [outmarsh [*] [-]]
```

Modifiers — these are the heart of c2hs's design and the conceptual
vocabulary we want to import (not the syntax):

| Modifier | Meaning |
|----------|---------|
| `*` after marshaller | Marshaller is `IO`-actioned, bracketed: `a -> (b -> IO r) -> IO r` (in) or `c -> IO h` (out) |
| `-` after marshaller | Suppress this slot from the Haskell signature (allocated for us or discarded) |
| `&` after type | One Haskell argument feeds **two** consecutive C slots |
| `%` after type | Pass a C struct bare (not by pointer); emits a small C wrapper |
| `+`, `+S`, `+N` | Allocate scratch space sized natively / by `sizeOf` / by `N` bytes; result appended to the tuple |
| `*-` on out | Effectful action whose result is discarded but whose effect is kept (error-check pattern) |

The Haskell return tuple of the generated function is the (ordered)
out-marshallers whose result is not suppressed by `-`, with the C return
value (if non-void) prepended.

**c2hs's default-marshaller table** (from `src/C2HS/Gen/Bind.hs`
`lookupDftMarshIn`/`lookupDftMarshOut`):

- `Bool` ↔ integral: `fromBool` / `toBool`
- integral Haskell ↔ integral C: `fromIntegral`
- floating Haskell ↔ floating C: `realToFrac`
- `Char` ↔ `CChar` / `CUChar` / `CSChar`: `castCharToC{S,U,}Char` and inverse
- `String` ↔ `char*`: `withCString*` / `peekCString*`
- `String` ↔ `char* + len` (`&`): `withCStringLenIntConv*` / `peekCStringLenIntConv*`
- `Storable a => a` ↔ `Ptr a`: `with*` / `peek*`
- `Ptr a` ↔ `void*`: `id` (no-op)
- enum hooks: `fromIntegral . fromEnum` / `toEnum . fromIntegral`
- foreign-pointer hooks: `withForeignPtr*` / `newForeignPtr_` (+ finalizer)

Worked example from the c2hs docbook (verbatim):

```c
void gtk_notebook_query_tab_label_packing(GtkNotebook *, GtkWidget *,
                                          gboolean *, gboolean *,
                                          GtkPackType *);
```

```haskell
{#fun notebook_query_tab_label_packing as ^
   `(NotebookClass nb, WidgetClass cld)' =>
   { notebook `nb',
     widget   `cld',
     alloca-  `Bool'     peekBool*,
     alloca-  `Bool'     peekBool*,
     alloca-  `PackType' peekEnum* } -> `()' #}
```

generates

```haskell
notebookQueryTabLabelPacking :: (NotebookClass nb, WidgetClass cld)
                             => nb -> cld -> IO (Bool, Bool, PackType)
```

The three suppressed-input (`alloca-`) arguments become three result-tuple
components via their `peek*` out-marshallers.

**c2hs's `{#default#}` hook** (added in 0.23.1) lets the user register
module-scoped default marshallers per `(HsTy, CTy)` pair:

```
{# default in `HsTy' = inMarshallerName* #}
{# default out `HsTy' = outMarshallerName* #}
```

This is c2hs's *extensibility hatch* — and notably, it is type-pair
lookup, not subsumed by a type class. The successor design in §6 replaces
this table with a proper type class.

**What we should take from c2hs.** The modifier system is the right
*conceptual* decomposition: every arg position carries (a) an
in-marshaller, (b) an out-marshaller, (c) arity flags, (d) discard
options. The right *encoding* for Haskell — without a custom preprocessor
— is a class hierarchy that captures (a)+(b) per type and lets the user
opt in to (c)+(d) via named newtype wrappers.

**What we should NOT take.** The DSL itself: `{#fun ... #}` is a custom
preprocessor with no IDE support, no HLS, no GHCi exploration, and a
syntax the c2hs docs themselves describe as "rather arcane." Issue #8's
own reframing (edsko's comment, 2026-05-20) is explicit: *"tool-assisted
high-level binding first"* — i.e., the user writes the wrapper in ordinary
Haskell and the library supplies the combinators.

Ezyang's blog post "Call and fun: marshalling redux"
(http://blog.ezyang.com/2010/06/call-and-fun-marshalling-redux/), linked
from issue #8 directly, makes the same point. He identifies the verbose
generated output (`cFloatConv . sinf'_ . cFloatConv`), the brittleness of
type-directed defaults, and the lack of generalisation beyond the
hard-coded 1-and-2 arities of `&`. The lesson: **every behaviour should be
explicit and overridable** in the high-level API.

c2hs's algebraic structure is, in a single sentence, *a fixed-arity,
table-driven, first-order language for assembling `Codensity IO` chains
around a foreign call*. A modern Haskell design replaces each row of the
table with a `Marshal` instance and adds *composition* on top, which c2hs
fundamentally lacks.

### 3.3 OCaml `ctypes` (Yallop)

The most influential modern C-binding library outside Haskell. Two ideas
are directly relevant.

**(1) Function descriptions as values, not syntax.**

```ocaml
let sigaddset =
  foreign "sigaddset" (ptr sigset_t @-> int @-> returning int)
```

`@->` is right-associative; the value `(ptr sigset_t @-> int @-> returning
int)` has type `(sigset_t ptr -> int -> int) fn`. There is no
preprocessor; the library uses GADTs to encode arity.

**(2) `view` — explicit bidirectional custom conversion.**

```ocaml
val view : read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ

let string =
  view ~read:string_of_char_ptr ~write:char_ptr_of_string (ptr char)
```

`view` is the OCaml-ctypes analog of the L1 conversion class we want. Note
the asymmetry: `view` is *pure* on both sides; bracket semantics
(`alloca`/`free`) are baked into the runtime's GC. For Haskell we need
explicit bracket support, so the analog is `Codensity IO` rather than
`(a → b, b → a)`. The shape is the same.

**(3) The Yallop/Sheets/Madhavapeddy paper "A Modular Foreign Function
Interface"** (SCP 164, 2018) generalises this further: a binding
*description* is parameterised over a *backend*. The OCaml `Cstubs`
system takes the same `BINDINGS (F : FOREIGN)` functor and instantiates it
with `libffi`, with statically-emitted C stubs, with stub-discovery code,
or with `Lwt`-wrapping. For Haskell, the moral analog is a type-class
parameterisation of the binding: the same `Marshal` instances can drive
PP / TH / SHs backends, or testing / mocking layers. This is a strong
endorsement of the type-class direction.

**What we should take.** The `read`/`write` pair is exactly the L1
class shape. The "function description as a value" idea is *not* what
we want at the user level (the user's prompt asks for typeclasses); but
it points at the right *algebraic structure* that the typeclasses encode.

**What ctypes does NOT solve.** The "1 OCaml value → multi C args" case
is left to the user: in `ctypes` you'd describe the raw C function with N
slots and then write an OCaml wrapper that pre-processes the args. There
is no native `&` modifier. So ctypes is no help on §2.3 specifically.

### 3.4 `inline-c` (FPComplete)

Quasi-quoted C with Haskell anti-quotation:

```haskell
countBits :: BS.ByteString -> IO CInt
countBits bs = [C.block| int {
  int i, bits = 0;
  for (i = 0; i < $bs-len:bs; i++) {
    bits += __builtin_popcount($bs-ptr:bs[i]);
  }
  return bits;
} |]
```

The interesting abstraction is `Context`, a monoid of marshalling competence:

```haskell
data Context = Context
  { ctxTypesTable   :: TypesTable   -- C type → Haskell type
  , ctxAntiQuoters  :: AntiQuoters  -- $vec-ptr:(int *xs) etc.
  , ...
  } deriving (Semigroup, Monoid)
```

Users assemble contexts as `baseCtx <> bsCtx <> vecCtx <> funCtx`. Each
new package contributes new types and new anti-quoters without modifying
the core. The "1 Hs → 2 C" case is solved by **using two anti-quoters
that refer to the same Haskell variable**:

```haskell
$bs-ptr:bs    -- pointer view of bs :: ByteString
$bs-len:bs    -- length view of the same bs
```

**What we should take.** The Monoid pattern is excellent — high-level
marshalling competence should be a *value*-level, modularly extensible
artifact. In a type-class design, this corresponds to letting users add
new `Marshal` / `Argument` instances *in their own module* without
touching the central library — i.e., it argues against fundepy-style
closed type families and in favour of an open-world class hierarchy.

The "two anti-quoters, same source" pattern is the inline-c answer to the
multi-slot problem; for a generated-binding tool, it translates to a
single `Argument` instance whose `ArgC` type family yields a tuple.

### 3.5 `bindings-DSL`

A pure-CPP DSL on top of `hsc2hs`. Does no marshalling; strictly mirrors C.
Mentioned for completeness; not architecturally informative.

### 3.6 Production handwritten bindings

Several production Haskell binding libraries have *independently re-invented
the same L1 class pair*, which is strong evidence that this layer is real:

- **`vulkan`** (expipiplus1): `ToCStruct` / `FromCStruct` with
  `pokeCStruct :: Ptr a -> a -> IO b -> IO b`,
  `withCStruct :: a -> (Ptr a -> IO b) -> IO b`,
  `peekCStruct :: Ptr a -> IO a`.
- **`OpenGL`** (ekmett ecosystem): `StateVar a` for paired `glGet`/`glSet`
  patterns; `HasGetter` / `HasSetter` / `HasUpdate` MPTCs.
- **`OpenGLRaw`**: `MarshalHostType` / `MarshalGLType` classes.
- **`fficxx`**: an `IsA`-style typeclass hierarchy reflecting C++ subtyping.
- **`haskell-gi`**: a `Marshallable` class parameterised over `GValue`.
- **`bytestring`**: `useAsCString` / `useAsCStringLen` — the same shape as
  the c2hs `*` and `*&` modifiers.

The recurring re-invention of L1 is the **single strongest argument** that
`hs-bindgen` should ship the canonical one.

### 3.7 Rust `bindgen` + handwritten "-sys" / "safe" crates

The Rust ecosystem has consciously converged on a *two-crate* pattern:
`foo-sys` (low-level, generated) + `foo` (high-level, hand-written). No
attempt is made to auto-generate the high-level layer — the consensus is
that the choices are too domain-specific. **The lesson:** provide a strong
vocabulary for hand-writing, then if generation becomes desirable, layer it
on top.

### 3.8 The Well-Typed "Purgatory" blog post

Real-world experience report binding Rust through C in Haskell. Documents:

- **Pointer + mutable-length** (`Ptr Word8`, `Ptr size_t`) for serialised
  return values; their `withBorshVarBuffer`, `allocFixedBuffer`,
  `allocMaxBuffer` helpers.
- **`Tag` phantom-type trick** for orphan-instance avoidance — `ToHaskell
  t` / `FromHaskell t` typeclass pairs.
- **`HaskellSize` / `HaskellMaxSize`** — compile-time-known size bounds.

This validates §2.3 and §2.6 as *the* central patterns; the rest is layout
detail. The phantom-tag trick will reappear in §7 (open question #3) as
one option for resolving instance-ambiguity for "by-pointer" types.

### 3.9 Theoretical perspectives

Both research reports converge on the same observation: a marshaller is a
**profunctor optic in the Codensity (or Resource) monad over IO**.
Specifically the shape

```haskell
data Marshaller hi c = Marshaller
  { send :: forall r. hi -> (c -> IO r) -> IO r
  , recv :: c -> IO hi
  }
```

is:

- contravariant in `hi` (pre-compose `send` via `lmap`);
- covariant in `c` (post-compose `recv` via `rmap`);
- with `IO`-bracket structure on the `send` side.

Composing marshallers in the `c`-direction (the "&" pattern) corresponds
to a product in the `c`-side type — making a heterogeneous list / tuple.
The natural algebra over these is the **free applicative on `Marshaller`s
indexed by source Haskell type** — a recipe both research reports arrived
at independently.

Relevant references:

- Xia, Orchard, Wang, **"Composing Bidirectional Programs Monadically"**
  (Haskell 2019) — formalises the biparser-as-profunctor view.
- Yallop, Sheets, Madhavapeddy, **"A Modular Foreign Function Interface"**
  (SCP 2018) — the OCaml ctypes design, modularity over backends.
- McBride, **"Faking It"** (JFP 2002) — length-indexed `Vec` for static
  array lengths (relevant if we want size-typed array marshallers).
- Edward Yang's blog posts ("Call and fun: marshalling redux";
  "Principles of FFI API design") — practical taxonomy.

The point is not that the proposed design must literally be a
`Profunctor` instance (it can be, but doesn't have to be). The point is
that the shape of L1 (`send` + `recv` per type) is established and
correct; we are not inventing.

---

## 4. The current `hs-bindgen` building blocks

What the project already ships, verbatim:

| Module | Purpose | What it gives us |
|--------|---------|------------------|
| `HsBindgen.Runtime.Marshal` | Generalises `Storable` | `StaticSize`, `ReadRaw`, `WriteRaw`, `EquivStorable`, `with`, `withZero`, `new`, `newZero` |
| `HsBindgen.Runtime.Internal.HasFFIType` | Newtype-erasing FFI bridge | `HasFFIType` class, `ViaNewtype` / `ViaCoercible` / `ViaFFIType` deriving helpers |
| `HsBindgen.Runtime.Internal.FunPtr.Class` | Callbacks | `ToFunPtr`, `FromFunPtr`, `withFunPtr` |
| `HsBindgen.Runtime.PtrConst` | `const T*` | `PtrConst`, `unsafeToPtr`, `unsafeFromPtr` |
| `HsBindgen.Runtime.IsArray` | Pointer-into-array bracket | `IsArray`, `withElemPtr` |
| `HsBindgen.Runtime.IncompleteArray` | Flex array members / `T[]` | `IncompleteArray`, `peekArray`, `pokeArray` |
| `HsBindgen.Runtime.ConstantArray` | `T[N]` | `ConstantArray` |
| `HsBindgen.Runtime.HasCField` | Named struct field access | `HasCField`, `peek`, `poke`, `readRaw`, `writeRaw`, `offset` |
| `HsBindgen.Runtime.HasCBitfield` / `BitfieldPtr` | Bit-fields | `HasCBitfield`, `mkBitfieldPtr` |
| `HsBindgen.Runtime.CEnum` | Enum marshalling | `CEnum`, `SequentialCEnum`, `AsCEnum` / `AsSequentialCEnum` deriving helpers |
| `HsBindgen.Runtime.Block` | Apple blocks | `Block` opaque wrapper |
| `HsBindgen.Runtime.FLAM` | Structs with flexible array member | `WithFlam`, `Offset`, `NumElems` |
| `HsBindgen.Runtime.Internal.SizedByteArray` | Unions | `zeroUnionValue` |

Two observations matter for the design:

1. **`HasFFIType` is already a profunctor-shaped class.** The instance for
   `(->)` lifts conversion contravariantly through arrows:

   ```haskell
   instance (HasFFIType a, HasFFIType b) => HasFFIType (a -> b) where
     toFFIType   f = \x -> toFFIType   (f $ fromFFIType x)
     fromFFIType f = \x -> fromFFIType (f $ toFFIType   x)
   ```

   (`hs-bindgen-runtime/src/HsBindgen/Runtime/Internal/HasFFIType.hs:233-238`).
   That is *literally* `dimap fromFFIType toFFIType f`. So the algebraic
   shape we want is already partially in place. What `HasFFIType` does NOT
   do is allow effects (IO bracket) or one-to-many slot mapping.

2. **`ToFunPtr` / `FromFunPtr`** is **exactly L1 specialised to function
   pointers**, with `withFunPtr` being the bracket form. Generalising this
   to all marshalled types is the proposal. The naming convention, class
   shape, and deriving discipline are the model.

---

## 5. Design space — three competing approaches

### Plan A — A single bidirectional class `Marshal`

```haskell
class Marshal hi where
  type Foreign hi :: Type
  withMarshal :: hi -> (Foreign hi -> IO r) -> IO r
  peekMarshal :: Foreign hi -> IO hi
```

**Strengths.** Single class, instances obvious for `String`, `ByteString`,
`Bool`, `Maybe Ptr`. Aligns with `ToFunPtr`/`FromFunPtr` shape. Trivial
deriving-via.

**Weaknesses.** Fundamentally L1 only. Cannot express the "1 Hs → 2 C"
case at all because `Foreign hi` is a single type. Would force users to
synthesise tuples via newtypes (`StringWithLen` ↔ `(Ptr CChar, CSize)`),
which is exactly the friction we're trying to eliminate. The user's
prompt is explicit that we need first-class support for this pattern.

### Plan B — Value-level binding descriptors (OCaml `ctypes` style)

```haskell
data Sig args where
  Ret   :: Marshaller hi c -> Sig (IO hi) (IO c)
  (:->) :: Marshaller hi c -> Sig rest1 rest2 -> Sig (hi -> rest1) (c -> rest2)

foreign_ :: lo -> Sig hi lo -> hi
```

A binding spec is a value `Sig (String -> Ecc -> IO QRCode) (Ptr CChar ->
QrEcc -> IO ...)`. A combinator consumes the spec and the low-level
binding to produce a high-level binding.

**Strengths.** Most expressive: natural fit for the multi-slot patterns
(make `Marshaller hi (HList cs)`); cleanest algebraic structure (the GADT
literally describes a function arrow). Matches the OCaml `ctypes`
template — and the Yallop modular-FFI thesis ("description as value,
backend as parameter").

**Weaknesses.** Heavy on type-level machinery (the `Sig` GADT has two
type-level lists). Less discoverable: users need to learn a new
mini-language even though it's pure Haskell. Harder to derive defaults
from existing class instances — each binding requires assembling a `Sig`
value. **Diverges from the user's explicit ask** for a typeclass-based
design.

### Plan C — Layered classes (recommended)

A small type-class hierarchy with explicit slots, plus a single combinator
`hl`. Each layer matches a real conceptual unit:

```haskell
-- L1: single Hs ↔ single C slot, bracket-style + bidirectional.
class Marshal hi where
  type Foreign hi :: Type
  withMarshal :: hi -> (Foreign hi -> IO r) -> IO r
  peekMarshal :: Foreign hi -> IO hi

-- L2: one Hs value may consume multiple C slots (the c2hs '&' case).
class Argument hi where
  type ArgC hi :: [Type]
  withArg :: hi -> (HList (ArgC hi) -> IO r) -> IO r

class Result hi where
  type ResScratch hi :: [Type]   -- slots we pre-allocate (out parameters)
  type ResRet     hi :: Type     -- the C-side return value
  peekRes :: HList (ResScratch hi) -> ResRet hi -> IO hi

-- L3: glue them together with a single overloaded combinator.
hl :: Call hi lo => lo -> hi
```

**Strengths.**
- Each layer matches a real conceptual unit.
- L1 is the 90%-case: a one-line class with the *same shape* as the
  existing `ToFunPtr`/`FromFunPtr`, so the codebase has one consistent
  vocabulary.
- Composition is structural: an `Argument` for `(String, Int)` is built
  from `Marshal String` and `Marshal Int` by a single deriving-via newtype.
- Existing in-tree classes slot in: `Marshal CBool = Bool`,
  `Marshal CInt = Int`, `Marshal CChar = Char`, `Marshal (PtrConst CChar)
  = String`, `Marshal (CEnumZ a) = a`. The L1 class instances **are** the
  c2hs default-marshaller table — but typeclass-resolved, not table-driven.
- Users can stop at any layer. For 90% of bindings, `instance Marshal` +
  a single `hl` call is enough.

**Weaknesses.**
- Type-level lists (`'[Type]`) and an `HList`-like construct are needed
  for L2. Modern GHC handles this well but it's a meaningful complexity
  tax.
- The "1 Hs result reconstructed from multiple C outputs" case (§2.6)
  requires `Scratch` allocation discipline tied to `Result` — necessarily
  somewhat involved, but no worse than c2hs's `alloca-` + `peek*`.
- Failing-instance error messages on `hl` will need a `TypeError` polish
  pass.

### Verdict

**Plan C** is the recommended approach. Reasons:

1. It matches the user's own sketch in the prompt — type-class-based,
   with "a single marshaller that maps single Hs arg to two C args" — without
   introducing a value-level DSL the user did not ask for.
2. It piggy-backs on the existing `HasFFIType` / `ToFunPtr` infrastructure,
   so most L1 instances are one-liners via deriving-via.
3. It is layered: simple cases stay simple. A user wrapping a
   `getter :: Ptr S -> IO CInt` only needs `instance Marshal Int` (already
   provided). Only the gnarly `String ↔ (Ptr CChar, CSize)` cases need
   L2.
4. The "stop at any layer" property maps to the project's stated roadmap:
   Milestone 2.5 is library support for *hand-written* bindings;
   Milestone 3 is generation atop, possibly reusing the same classes.
5. Plan B can still be *built on top of Plan C* later (a `Sig` GADT can
   be a thin wrapper over the same instances); the reverse direction is
   harder.

The trade-off on Plan B's value-level DSL is real — if at some future
point a user wants `ctypes`-style `(string @-> int @-> returning int)`
ergonomics, that can be built on top of Plan C's classes; the inverse is
not as clean.

---

## 6. Recommended design — details

### 6.1 Layer 0 — what already exists

```haskell
class HasFFIType a where
  type ToFFIType a :: FFI.FFIType
  toFFIType   :: a -> FFIType a
  fromFFIType :: FFIType a -> a
```

`HasFFIType` continues to do its current job — erasing newtypes for FFI
declarations. The proposal does not touch it.

### 6.2 Layer 1 — `Marshal` (the single-slot class)

```haskell
-- New: HsBindgen.Runtime.Marshal.HighLevel
class Marshal hi where
  type Foreign hi :: Type

  -- Bracket the high-level value to give the C side a pointer/value.
  -- For pure conversions (CInt, CBool, enums, ...) this just unwraps.
  -- For bracketed conversions (String, ByteString, ...) it allocates.
  withMarshal :: hi -> (Foreign hi -> IO r) -> IO r

  -- Reconstruct the high-level value from the C side.
  peekMarshal :: Foreign hi -> IO hi
```

`Foreign hi` is a *type family*, not a parameter — this is the "one Hs
value, one C slot" tier. The pair `(withMarshal, peekMarshal)` is a
bidirectional bracket, isomorphic in shape to OCaml-ctypes's `view`.

**Pure-conversion instances** can be derived from `HasFFIType`:

```haskell
newtype Pure a = Pure a

instance HasFFIType a => Marshal (Pure a) where
  type Foreign (Pure a) = FFIType a
  withMarshal (Pure x) k = k (toFFIType x)
  peekMarshal y          = pure (Pure (fromFFIType y))
```

So `deriving Marshal via (Pure CInt)` gives `instance Marshal CInt` for
free; same for every primitive C type.

**Bracket-style instances** are the c2hs `*` cases:

```haskell
instance Marshal String where
  type Foreign String = Ptr CChar
  withMarshal s k     = withCString s k
  peekMarshal p       = peekCString p

instance Marshal ByteString where
  type Foreign ByteString = Ptr CChar
  withMarshal bs k        = BS.useAsCString bs k
  peekMarshal p           = BS.packCString p
```

**Optional pointers** (the `NULL` ↔ `Maybe` case, §2.9). This needs a
class refinement (`MarshalNullable`) because we cannot claim `Marshal
(Maybe Bool)`:

```haskell
class Marshal a => MarshalNullable a where
  isNullForeign :: Foreign a -> Bool
  nullForeign   :: Foreign a

instance MarshalNullable a => Marshal (Maybe a) where
  type Foreign (Maybe a) = Foreign a
  withMarshal Nothing  k = k nullForeign
  withMarshal (Just x) k = withMarshal x k
  peekMarshal p
    | isNullForeign p = pure Nothing
    | otherwise       = Just <$> peekMarshal p
```

**`CEnum` instances** are immediate via deriving-via:

```haskell
-- A user writing
newtype Qrcodegen_Ecc = Qrcodegen_Ecc { unwrap :: CUInt }
  deriving (Marshal) via Pure Qrcodegen_Ecc
-- (CEnum already provides toCEnum / fromCEnum.)
```

So `Qrcodegen_Ecc`, `Qrcodegen_Mask`, etc. are L1-marshallable with no
hand-written code beyond a single `deriving` clause.

**ForeignPtr / handle instances** are also one-liners:

```haskell
instance Marshal (ForeignPtr a) where
  type Foreign (ForeignPtr a) = Ptr a
  withMarshal = withForeignPtr
  peekMarshal = newForeignPtr_      -- user attaches finaliser if desired
```

### 6.3 Layer 2 — `Argument` and `Result` (multi-slot)

This is the layer that captures the "1 Hs ↔ N C" patterns. The user's
prompt asks for it explicitly.

```haskell
-- Sketch; could use vec-lazy already in the project's dep tree.
data HList :: [Type] -> Type where
  HNil  :: HList '[]
  (:::) :: a -> HList xs -> HList (a ': xs)

class Argument hi where
  type ArgC hi :: [Type]
  withArg :: hi -> (HList (ArgC hi) -> IO r) -> IO r

class Result hi where
  type ResScratch hi :: [Type]    -- pre-allocated out parameter slots
  type ResRet     hi :: Type      -- C-side return value type
  peekRes :: HList (ResScratch hi) -> ResRet hi -> IO hi
```

**Default `Argument` from `Marshal`.** Most arguments are single-slot, so
we provide a generic instance via an explicit newtype to keep instance
resolution unambiguous:

```haskell
newtype Single hi = Single hi

instance Marshal hi => Argument (Single hi) where
  type ArgC (Single hi) = '[Foreign hi]
  withArg (Single x) k = withMarshal x $ \y -> k (y ::: HNil)
```

In practice the `Call` class picks the `Single` path by default when a
plain `Marshal` instance exists; users only need to wrap explicitly when
they want multi-slot.

**Alternative — fold `Argument` into `Marshal` via default methods**
(flagged by the Phase 3 evaluator as worth investigating). The
`Single` / `SingleRes` newtypes are arguably boilerplate. An alternative
shape is to give `Marshal` an associated type family for the slot list
with a single-slot default:

```haskell
class Marshal hi where
  type ArgC hi :: [Type]
  type ArgC hi = '[Foreign hi]              -- default: one slot

  withArg :: hi -> (HList (ArgC hi) -> IO r) -> IO r
  default withArg
      :: ('[Foreign hi] ~ ArgC hi) => hi -> (HList '[Foreign hi] -> IO r) -> IO r
  withArg x k = withMarshal x $ \y -> k (y ::: HNil)

  withMarshal :: hi -> (Foreign hi -> IO r) -> IO r
  peekMarshal :: Foreign hi -> IO hi
```

Then a single-slot user writes `instance Marshal Bool` and gets the
default `ArgC = '[CBool]` and `withArg = withMarshal`; a multi-slot user
overrides `ArgC` and `withArg`. **Trade-off**: removes the `Single`
newtype but couples L1 and L2 in one class — which is fine if L2 is
always meaningfully derivable from L1, less fine if we ever want a type
that has `Argument` but not `Marshal` (e.g., a writer-only `Out`-shaped
input). My current judgement is the unified-class shape is *probably*
cleaner; I'd validate it during implementation by trying to write
`Argument` instances that don't have a sensible `Marshal` counterpart and
seeing if the design tears. The choice is not load-bearing — both shapes
yield the same user-visible API for `hl`.

**Multi-slot instances.** Not derivable; written once per pattern, then
reused everywhere.

```haskell
-- The c2hs '&' pattern, for ByteString.
newtype PtrLen a = PtrLen a

instance Argument (PtrLen ByteString) where
  type ArgC (PtrLen ByteString) = '[Ptr CChar, CSize]
  withArg (PtrLen bs) k =
    BS.useAsCStringLen bs $ \(p, n) ->
      k (p ::: fromIntegral n ::: HNil)

-- Same pattern for the project's IncompleteArray.
instance Storable a => Argument (PtrLen (IncompleteArray a)) where
  type ArgC (PtrLen (IncompleteArray a)) = '[Ptr a, CSize]
  withArg (PtrLen arr) k =
    IsA.withElemPtr arr $ \p ->
      k (p ::: fromIntegral (VS.length (IA.toVector arr)) ::: HNil)
```

Composing two arguments — when the user's high-level function has two
parameters — uses a derived instance for pairs and a type-level
list-concat:

```haskell
type family Concat (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Concat '[]       ys = ys
  Concat (x ': xs) ys = x ': Concat xs ys

instance (Argument a, Argument b) => Argument (a, b) where
  type ArgC (a, b) = Concat (ArgC a) (ArgC b)
  withArg (x, y) k = withArg x $ \as -> withArg y $ \bs -> k (happend as bs)
```

(`happend` is the term-level concatenation; standard machinery, ~10 LOC.)

**`Result` instances.**

The simplest case (most common — single C return value):

```haskell
newtype SingleRes hi = SingleRes hi

instance Marshal hi => Result (SingleRes hi) where
  type ResScratch (SingleRes hi) = '[]
  type ResRet     (SingleRes hi) = Foreign hi
  peekRes HNil y = SingleRes <$> peekMarshal y
```

The out-parameter case (§2.4 — `srp6_group_size`):

```haskell
newtype Out hi = Out hi

instance (Marshal hi, StaticSize (Foreign hi))
      => Result (Out hi) where
  type ResScratch (Out hi) = '[Ptr (Foreign hi)]
  type ResRet     (Out hi) = CInt   -- status; ignored by default
  peekRes (resPtr ::: HNil) _status = Out <$> (peekMarshal =<< peek resPtr)
```

The buffer + length case (§2.6):

```haskell
data Buf a = Buf { bufCap :: CSize, bufResult :: IncompleteArray a }

instance Storable a => Result (Buf a) where
  type ResScratch (Buf a) = '[Ptr a, Ptr CSize]
  type ResRet     (Buf a) = CInt   -- status
  peekRes (bufP ::: lenP ::: HNil) _status = do
    actualLen <- peek lenP
    arr <- IA.peekArray (fromIntegral actualLen) (IA.toPtr bufP)
    pure (Buf actualLen arr)
```

The error-as-exception variant (c2hs's `*-` modifier; §2.8). Users opt in
by wrapping the result type:

```haskell
newtype Throws e r = Throws r

instance (FromCInt e, Exception e, Result r) => Result (Throws e r) where
  type ResScratch (Throws e r) = ResScratch r
  type ResRet     (Throws e r) = CInt
  peekRes scratch n
    | n < 0     = throwIO (fromCInt n :: e)
    | otherwise = Throws <$> peekRes scratch (...)
```

**Scratch allocation discipline** is a third small class paired with
`Result`:

```haskell
class Scratch (xs :: [Type]) where
  withScratch :: (HList xs -> IO r) -> IO r

instance Scratch '[] where
  withScratch k = k HNil

instance (Scratch xs, StaticSize a) => Scratch (Ptr a ': xs) where
  withScratch k =
    Marshal.with @a undefined $ \p ->
    withScratch              $ \ys ->
    k (p ::: ys)
```

The in/out length case (§2.5) is a special `Scratch` whose initial poke
seeds the capacity. It's parametric on the chosen capacity, so it's a
newtype wrapping a `CSize`:

```haskell
data InOutLen = InOutLen CSize
-- A custom Scratch + Result pair for buffer-with-capped-length;
-- factored out for reuse.
```

### 6.4 Layer 3 — the `hl` combinator

The user-facing API is one overloaded combinator driven by a class
`Call`:

```haskell
class Call hi lo where
  hl :: lo -> hi

-- Argument step
instance ( Argument a
         , Call rest lo'
         , lo ~ ApplyArgC (ArgC a) lo'
         )
      => Call (a -> rest) lo where
  hl f a = withArg a $ \cs -> hl (applyHList cs f :: rest)

-- Result step
instance ( Result r
         , Scratch (ResScratch r)
         , callLo ~ ApplyArgC (ResScratch r) (IO (ResRet r))
         )
      => Call (IO r) callLo where
  hl f =
    withScratch $ \scratch ->
      peekRes scratch =<< applyHList scratch f
```

`ApplyArgC` prepends each slot to the low-level function's arrow chain;
`applyHList` is the value-level analog. (Pseudocode — the actual elab
needs a closed type family on the shape of `hi`. Standard servant-style
machinery.)

The usage is then:

```haskell
qrcodegen_encodeText :: PtrConst CChar -> Ptr Word8 -> Ptr Word8 -> Ecc -> CInt -> CInt
                     -> Mask -> CBool -> IO CBool
qrcodegen_encodeText = ... -- generated by hs-bindgen

qrEncodeText :: String -> IncompleteArray Word8 -> IncompleteArray Word8 -> Ecc
             -> Int -> Int -> Mask -> Bool -> IO Bool
qrEncodeText = hl qrcodegen_encodeText
```

### 6.5 Worked example A — `srp6_group_size` (out parameter)

C declaration:

```c
int botan_srp6_group_size(const char *group_id, size_t *group_size);
```

Generated low-level binding (today):

```haskell
botan_srp6_group_size :: PtrConst CChar -> Ptr CSize -> IO CInt
```

Hand-written wrapper today
(`examples/botan/hs-project/src/Botan.hs:237-243`):

```haskell
srp6GroupSize :: GroupId -> IO CSize
srp6GroupSize groupId =
    withCString (groupIdString groupId) $ \groupIdPtr ->
    alloca $ \resPtr -> do
      throwErrnoIfNegative "botan_srp6_group_size" $
        botan_srp6_group_size (PtrConst.unsafeFromPtr groupIdPtr) resPtr
      peek resPtr
```

With the proposed design:

```haskell
srp6GroupSize :: GroupId -> IO (Out (Throws BotanError CSize))
srp6GroupSize = hl botan_srp6_group_size
```

Annotated step-by-step:

1. `Marshal GroupId`: a pure conversion to `PtrConst CChar` via
   `groupIdString` plus `withCString` (one user-written instance).
2. The result type `Out (Throws BotanError CSize)` triggers two nested
   `Result` instances: the inner `Throws` converts the `CInt` status into
   an exception; the outer `Out` allocates a `Ptr CSize`, runs the call,
   peeks the result.

The user-visible signature elides every plumbing detail. From 6 lines of
glue to 1 line of binding declaration.

### 6.6 Worked example B — `srp6_server_session_step1` (the full picture)

C declaration:

```c
int botan_srp6_server_session_step1(
    botan_srp6_server_session_t   session,
    const uint8_t *               verifier,  size_t verifier_len,
    const char *                  group_id,
    const char *                  hash_id,
    botan_rng_t                   rng,
    uint8_t *                     B,         size_t *B_len);
```

Generated low-level binding signature:

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

Hand-written wrapper today
(`examples/botan/hs-project/src/Botan.hs:109-136`): **27 lines** of
`withCString` / `allocaBytes` / `alloca` / `poke` / `peek` /
`fromIntegral` / `peekArray` / `throwErrnoIfNegative` plumbing.

With the proposed design:

```haskell
srp6ServerSessionStep1
  :: ServerSession
  -> PtrLen Verifier
  -> GroupId -> HashId
  -> RNG
  -> CSize                  -- capacity for B
  -> IO (Buf Word8)
srp6ServerSessionStep1 = hl botan_srp6_server_session_step1
```

(With a `Throws` wrapper if error-as-exception is desired.) The instances
pulled in:

- `Marshal ServerSession`  — pure unwrap (`Pure` deriving via).
- `Argument (PtrLen Verifier)` — bracket via `IsA.withElemPtr` + slot pair.
- `Marshal GroupId`, `Marshal HashId` — bracket via `withCString . *_idString`.
- `Marshal RNG` — pure unwrap.
- `Marshal CSize` — identity (`Pure CSize`).
- `Result (Buf Word8)` — allocate `Ptr Word8` of given capacity + `Ptr
  CSize`, pre-poke capacity, peek both after the call.

The user writes:
- Five trivial `deriving Marshal via Pure ...` instances for the domain
  newtypes — they already wrote these as `newtype ServerSession = ...`.
- *One* `Argument (PtrLen Verifier)` instance — derivable via newtype
  from `Argument (PtrLen (IncompleteArray Word8))` (which lives in the
  library).
- Nothing for the buffer + capacity logic; it's handled by the generic
  `Result (Buf Word8)` and `Scratch` instances.

Estimated diff for the whole `Botan.hs` file: from ~320 lines to ~120
lines — roughly a **5× reduction** in glue, with each surviving line being
a domain-meaningful declaration rather than scaffolding.

### 6.7 Where to draw the line on automation

The user's prompt and the project's roadmap (Milestone 3) both contemplate
*generation* on top of the patterns. The recommended design supports that
naturally:

- The class hierarchy is the **vocabulary**. Tooling (a future
  `hs-bindgen` pass, or an external tool) chooses *which* `Marshal` /
  `Argument` / `Result` instances to use for each function. The user's
  prompt is explicit that this ticket is "tool-assisted high-level
  binding first": Milestone 2.5, not Milestone 3.
- Per-binding choices (`String` vs `ByteString` vs `Text` for `char *`;
  `Bool` vs `()` vs `Either ErrorCode ()` for `int` returns) are
  user-controlled — same way c2hs lets you override defaults. The classes
  themselves are agnostic; the user picks the Haskell type and the
  matching instance is selected.
- A future Milestone-3 generator reads its decisions from a *prescriptive
  binding spec* (the YAML mechanism already in
  `hs-bindgen/src-internal/HsBindgen/BindingSpec/Private/V1.hs`) and emits
  `instance Marshal Foo ...` declarations alongside the call wrappers.

---

## 7. Open design questions

Original open questions plus their resolution after team review. The
canonical resolved discussion is in
[`HIGH_LEVEL_DESIGN.md`](./HIGH_LEVEL_DESIGN.md) §5–§6; this section
preserves the original wording and the team's verdict per question.

1. **`MarshalNullable` granularity.** *Original draft proposed a
   sub-class for "by-pointer" types.* **Resolved**: collapse into a
   single `Marshal` class; the `Maybe` instance carries a `Foreign a ~
   Ptr _` (or generalised `IsPointerShaped`) constraint so that
   `Marshal (Maybe Bool)` is ill-typed at instance-resolution rather
   than failing at runtime. See `HIGH_LEVEL_DESIGN.md` §5.1.

2. **Default error handling.** *Should `int` returns auto-throw?*
   **Resolved**: kept as an explicit non-feature; the team prefers to
   start without exception semantics in the defaults. Users opt in via
   a `Throws ErrorType` wrapper around their `Result`. The c2hs / ezyang
   lesson stands. See `HIGH_LEVEL_DESIGN.md` §6.7.

3. **`Storable` vs `ReadRaw`/`WriteRaw` divergence.** *Original draft
   worried about needing `Storable a` for `peekArray` while the project
   prefers `ReadRaw` / `WriteRaw`.* **Resolved**: non-issue in practice.
   Any type the user defines and wants to use through the high-level
   library should derive `Storable` via `EquivStorable`, which the
   runtime already provides. There is no divergence we need to design
   around. See `HIGH_LEVEL_DESIGN.md` §6.6.

4. **Naming — does `Multi` read as output?** *Original draft proposed
   wrapper newtypes `PtrLen`, `Out`, `Multi`, `Buf` to signal slot
   shapes in user-facing signatures.* **Resolved** (with bigger
   pivot): the wrapper newtypes were leaking into user signatures
   (e.g. `hsDoThing :: PtrLen ByteString -> IO Int`) where the user
   actually wants `hsDoThing :: ByteString -> IO Int`. The design was
   reworked: multi-slot expansion now lives in a type-family on the
   conversion class itself (`AsC ByteString r = Ptr CChar -> CSize ->
   r`), so `ByteString` directly knows it occupies two C slots. The
   `Marshal` / `Argument` / `Result` class triple was replaced with a
   `ToC` / `FromC` pair mirroring the existing
   `ToFunPtr` / `FromFunPtr` shape. Out-parameter and buffer-output
   cases that can't be inferred from result types use explicit
   bracket-shaped combinators (`withOut`, `withBuf`), not wrappers.
   See `HIGH_LEVEL_DESIGN.md` §5.

5. **`HList` vs N-ary curried continuation.** *Original draft used
   `HList` slot lists.* **Resolved**: prefer N-ary curried
   continuations via an `ArgF` type family — `withArg :: hi -> (c1 -> c2
   -> ... -> IO r) -> IO r`. The user-facing continuation is just a
   curried function, no `HList` to construct or splat. `HList` retained
   as fallback if the elaboration turns out to be intractable. See
   `HIGH_LEVEL_DESIGN.md` §5.2.1.

6. **Module namespace.** *Original draft used
   `HsBindgen.Runtime.Marshal.HighLevel`.* **Resolved**: switch to a
   dedicated `HighLevel` namespace —
   `HsBindgen.Runtime.HighLevel.Marshal`,
   `HsBindgen.Runtime.HighLevel.Argument`, etc., with a
   `HsBindgen.Runtime.HighLevel.Prelude` convenience module. See
   `HIGH_LEVEL_DESIGN.md` §5.4.

7. **Pretty error messages — load-bearing, not polish.** *Original draft
   noted the `servant`-style error-message risk.* **Resolved (as
   open)**: the team agrees this is a real risk worth mitigating, but
   wants to validate the design first before investing in `TypeError`
   polish. Tracked as a deferred item; will be revisited if early
   prototypes confirm the error messages are unusable. See
   `HIGH_LEVEL_DESIGN.md` §6.1.

8. **Backend modularity.** *Original draft mentioned a Yallop-style
   modular FFI with mock backends.* **Resolved**: the "quickcheck-style
   mock" was a stray theoretical reference; not load-bearing. The
   actual requirement is that the same `Marshal` instances be usable
   by the Preprocess (PP) backend and the Template Haskell (TH)
   backend. The design supports this trivially because the classes
   are plain Haskell. See `HIGH_LEVEL_DESIGN.md` §5.6.

---

## 8. Plans considered — summary table

| Plan | Approach | Verdict |
|------|----------|---------|
| **A** — Single `Marshal` class only | Easy, but cannot express §2.3 / §2.4 / §2.6 multi-slot patterns | Rejected |
| **B** — Value-level `Sig` GADT (OCaml-ctypes style) | Cleanest theoretical fit; full expressivity. But diverges from the user's typeclass-based brief and is heavier on type-level lists than necessary | Rejected for now; can be layered atop **C** later |
| **C** — Layered classes (`Marshal` / `Argument` / `Result`) + generic `hl` | Matches the user's prompt, layered, opt-in, builds on existing `HasFFIType` / `ToFunPtr` infrastructure | **Recommended** |

The Plan C evaluation against the project's guiding principles:

- **Correctness.** Each pattern in §2 has a worked-out instance with
  clear semantics. `peekMarshal . withMarshal` is the round-trip law
  for L1; `Argument` / `Result` decomposition leaves no orphan slot.
- **Fit.** Mirrors the existing `ToFunPtr` / `FromFunPtr` shape exactly;
  reuses `HasFFIType` for pure conversions; uses the existing
  `IncompleteArray`, `IsArray`, `PtrConst` types directly.
- **Maintainability.** All conversion logic lives in one place per type
  (the L1 instance). Adding a new C library that exposes a new
  marshalling pattern means writing one or two instances, not modifying
  the central library — the `inline-c` lesson, applied to typeclasses.
- **Diff size.** ~800 LOC of new library code + ~200 LOC docs +
  conversion of one or two examples (`Botan.hs`, `c-qrcode/Main.hs`)
  for validation. The example conversions shrink user code by ~5×, so
  the up-front cost amortises quickly.
- **Style match.** Naming (`Marshal`, `Argument`, `Result`,
  `withMarshal`, `peekMarshal`) follows the existing in-tree
  conventions (`with`/`peek` prefixes; qualified imports).

### Devil's advocate

The strongest arguments against Plan C, considered and answered:

- *"Plan B is theoretically cleaner — why not bite the bullet?"* The
  user's prompt explicitly asks for typeclasses. Plan C is the
  type-class-flavoured way to encode the same algebra. Plan B can sit on
  top of Plan C if desired.
- *"Too much type-level machinery for the simple case."* The simple
  (single-slot) case never exercises the L2 / `HList` machinery. The
  cost is paid only when users opt in to multi-slot patterns. We have
  in-tree precedent for similar elaboration in the binding-spec
  resolution.
- *"`hs-bindgen`'s generator could just emit `Foreign.Marshal.*` calls
  directly without a class hierarchy."* Yes, for one library. The
  hierarchy exists for the *user's* high-level layer, not for the
  generator. Without it, every user re-invents the same six classes
  (as `vulkan` / `OpenGL` / `OpenGLRaw` / `fficxx` already did).

### Phase 3 evaluator review (independent assessment)

An independent evaluator agent reviewed all three plans and reached the
same verdict — Plan C wins — for substantially the same reasons. Its
critique surfaced two refinements I have folded into the design:

1. **Front-load the `TypeError` polish on `Call`** rather than treating
   it as cosmetic clean-up. Reason: the `Call` / `ApplyArgC` / `HList`
   machinery is the same shape `servant` uses, and `servant`'s
   confusing-error-message reputation is a real, durable problem that
   the design *can* avoid by spending 1-2 days on `TypeError`-clad
   instance heads. See open question #7 (§7).

2. **Reconsider whether `Argument`/`Result` need to be classes separate
   from `Marshal`**. The `Single` / `SingleRes` wrapper newtypes feel
   like boilerplate at the call site. I have added (§6.3) a discussion
   of an alternative shape where `Marshal` carries an associated
   `ArgC :: [Type]` type family with a single-slot default, and L2 is
   just an override. Both shapes give the same user-visible API; the
   choice is best validated during implementation by attempting to
   write an `Argument` instance with no sensible `Marshal` counterpart
   and seeing if the design tears.

The evaluator also surfaced two devil's-advocate arguments worth
recording:

- *"L2 is over-engineered for the actual frequency of multi-slot
  cases."* Counter: the §2.13 table shows pointer+length, out-parameter,
  buffer+length, and in/out-length — four distinct multi-slot patterns
  — as either "must-cover" or "should-support". The class earns its
  keep starting at the third pattern; with only two it would not, but
  the catalog has more.

- *"`Call` + closed-type-families + `HList` + `ApplyArgC` is exactly
  the servant-style machinery whose error messages have plagued the
  ecosystem."* Real risk, partially mitigated by the narrower surface
  (one combinator, not a route DSL) and the enumerable failure modes
  (wrong arity / missing instance). The mitigation is refinement (1)
  above: `TypeError` polish must ship with the initial implementation,
  not later. If skimped, users will (correctly) blame the design.

The evaluator also considered two alternatives the document had not
fully explored:

- **Profunctor/optic-style design** (`Iso`/`Prism` between `hi` and
  `Foreign hi`, composed with `***`/`+++`). Theoretically elegant but
  imports a separate vocabulary (lens) foreign to the codebase, and
  bracket-style `withMarshal` doesn't fit cleanly into a pure optic
  without `ContT`-flavoured machinery. Not clearly better.

- **TH-driven elaboration** (greencard-style splice that emits the
  marshalling code from a target signature). Sidesteps type-level lists
  entirely, but moves errors to splice time, conflicts with the
  existing TH backend in the project, and forfeits the Milestone-2.5
  goal of library-only / hand-written-first. Not clearly better.

Both alternatives remain viable directions for a *future* second
iteration if Plan C runs into UX walls; neither displaces it as the
starting point.

---

## 9. Verification strategy

Since this is a research ticket and not an implementation ticket, the
"verification" is whether the proposal:

- **Covers every pattern in §2.** Done by inspection — each pattern in
  the catalog has a corresponding worked instance in §6.
- **Eliminates the friction in real user code.** Validated against the
  three concrete examples in the project (`Botan.hs`,
  `c-qrcode/app/Main.hs`, `libpcap/app/Pcap.hs`) by mentally porting
  them; §6.5 / §6.6 show the result for the two non-trivial cases.
- **Composes with the existing infrastructure.** L1's shape matches
  `ToFunPtr`/`FromFunPtr` exactly; L2's `Argument` for `IncompleteArray`
  uses `IsArray.withElemPtr` directly; `Marshal (PtrConst CChar)` uses
  the existing `PtrConst` machinery.
- **Leaves room for Milestone 3 (generation).** Yes: the class
  hierarchy is the *vocabulary* a future generator would emit
  instances for; no architectural lock-in.

If/when the design moves to implementation, the build/test plan is:

```bash
cabal build all                                # baseline
# new module HsBindgen.Runtime.Marshal.HighLevel
cabal test all --test-show-details=direct      # round-trip golden tests
# port examples/c-qrcode/hs-project to use hl
# port examples/botan/hs-project to use hl
cabal run --project-file=examples/c-qrcode/cabal.project ...
cabal run --project-file=examples/botan/cabal.project   ...
```

---

## 10. Risk assessment

- **Type-class instance resolution.** The `Call` class needs
  GHC to infer `lo` from `hi` and chase `ApplyArgC` through type-level
  lists. We have in-tree precedent (`HasFFIType (a -> b)`) and external
  precedent (`servant`'s `HasServer`); risk is *medium-low*. The mitigation
  is to put the elaboration logic in a closed type family rather than
  overlapping instances.
- **Error messages.** Failing instance lookup will be unpleasant. The
  mitigation is `TypeError`-laden helper instances that say "missing
  `Marshal Foo` — try `deriving via Pure Foo`".
- **Performance.** All the wrapping is bracket-based, so the only
  per-call cost is the same as the hand-written wrappers users write
  today. `withMarshal` for primitive types is `\x k -> k (coerce x)`
  after specialisation. Risk: *low*; mitigation: aggressive `INLINE`
  pragmas (we already do this in `HasFFIType` instances).
- **Backwards compatibility.** This is a pure addition; no existing
  generated code or runtime API changes. Risk: *low*.
- **Scope creep into Milestone 3.** Easy to slide from "design library"
  to "design generator." Mitigation: this proposal stays strictly within
  Milestone 2.5; generation is explicitly out of scope.

---

## 11. Estimated implementation cost

(Speculative since this is a research ticket; useful for sequencing.)

- New module `HsBindgen.Runtime.Marshal.HighLevel` (~150-250 LOC) —
  `Marshal`, `Argument`, `Result`, `Scratch`, `Call`, the `Pure` /
  `Single` / `PtrLen` / `Out` / `Buf` / `Throws` newtypes, the
  `Call` machinery.
- A handful of `instance Marshal ...` for `String`, `ByteString`, `Bool`,
  primitives, `Maybe`, enums, function pointers (the last via
  `withFunPtr`). ~100 LOC.
- Round-trip golden tests in `hs-bindgen-runtime/test/`. ~300 LOC.
- Manual page `manual/high-level/patterns.md` enumerating the §2 table
  and showing each `Marshal` instance with a worked example. ~200 LOC.
- Port `examples/botan/hs-project/src/Botan.hs` and
  `examples/c-qrcode/hs-project/app/Main.hs` to use the new API as
  validation. Net diff *negative* — the examples shrink.

**Total**: ~800 LOC new library code + ~200 LOC docs, with example
porting validating the design empirically. Roughly 1.5 developer-weeks,
distributed as:

- 1–2 days fleshing out the `Call` / `HList` machinery and proving it
  elaborates cleanly for the shapes in §6.5/§6.6.
- 1–2 days writing the canonical instances and deriving-via helpers.
- **1–2 days on `TypeError` / `Unsatisfiable` polish for `Call`.** Per
  the Phase 3 evaluator's flag (§8.devil's-advocate), this is *not*
  an "after the rest is done" task — the design's biggest UX risk is
  servant-style error-message rot, and the mitigation is upfront
  `TypeError`-clad instances that catch missing-`Marshal` and
  wrong-arity cases and produce focused diagnostics.
- 1–2 days porting the examples and using them to validate ergonomics.

---

## 12. Recommendations

1. **Adopt the layered class design** (Plan C, §6). The pivotal addition
   is the L2 `Argument` / `Result` pair, which is what makes the user's
   `withCStringLen`-style patterns expressible without forcing a
   value-level DSL.

2. **Don't reach for `ctypes`-style value descriptors yet.** They can
   sit on top of the classes later if ergonomics demand. The user's
   prompt explicitly favours typeclasses for this round.

3. **Don't bake in defaults that aren't explicit.** Every behavioural
   choice (String vs ByteString, exception vs Either, Bool vs CBool)
   is a user-side instance selection. This is the lesson c2hs's friction
   (ezyang's blog post) teaches.

4. **Treat `ToFunPtr` / `FromFunPtr` as the model.** They are the
   in-tree example of an L1 marshalling pair. Make `Marshal` analogous
   in shape, so the codebase has one consistent vocabulary.

5. **Validate against the existing examples** (`c-qrcode/Main.hs`,
   `Botan.hs`, `libpcap/Pcap.hs`) before locking the API. Each is a
   real-world test of whether the design eliminates the friction the
   issue is targeting.

6. **Keep extensibility through open type classes**, not closed type
   families. Users adding new high-level types should be able to add
   instances in their own modules; that's the `inline-c` Context-Monoid
   lesson translated to typeclasses.

This proposal stays within the project's stated Milestone 2.5 objective —
"library support for hand-written high-level bindings" — and leaves room
for Milestone 3 generation to be a *consumer* of the same vocabulary
later.
