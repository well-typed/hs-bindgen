# hs-bindgen-highlevel

Write type-safe high-level bindings for low level C FFI ones, declaratively.

## What this is for

Writing high-level bindings for C APIs is boring: allocate a buffer, pass a
`Ptr`, thread an out-parameter, read it back, check a status code, free what
the callee hands back.

This library offers a set of combinators the user composes to declaratively
specify the high-level function type. It handles all the marshalling and
unmarshalling plumbing, while keeping everything typed and safe. A binding is
a value built from combinators and then run:

```haskell
hsStrncmp :: String -> ByteString -> IO Int
hsStrncmp = toHighLevel ( input withCStringIn
                        $ input useAsByteStringLenIn
                        $ resultPure fromIntegral
                        ) c_strncmp
```

The spec reads top to bottom like an annotated version of the C function: one
`input` per Haskell argument, marshalled into the C argument(s) it needs, closed by
a conversion of the return value.

The approach is inspired by [c2hs](https://github.com/haskell/c2hs), however,
unlike `c2hs`, `hs-bindgen-highlevel` has no custom syntax, no code generation
step, just Haskell values that can be named, reused, and type checked.

## Quick start

`qrcodegen_encodeText` is a six-argument C function with a scratch (temporary)
buffer the caller allocates but never reads and an out-parameter that holds
the result:

```c
bool qrcodegen_encodeText(const char *text, uint8_t tempBuffer[], uint8_t qrcode[],
                          enum qrcodegen_Ecc ecl, int minVersion, int maxVersion,
                          enum qrcodegen_Mask mask, bool boostEcl);
```

The wrapper names only the two positions that need a decision. `scratchArray`
allocates the temp buffer and hides it from the type signature; `output` keeps
the QR code; `auto` fills everything else from the type signature:

```haskell
encodeText
  :: String -> Qrcodegen_Ecc -> Int -> Int -> Qrcodegen_Mask -> Bool
  -> IO (IncompleteArray Word8, Bool)
encodeText = toHighLevel
  ( auto                       -- text (String)
  $ scratchArray @Word8 maxLen -- tempBuffer: written, never read
  $ output qrCodeOut           -- qrcode: the out-parameter we keep
  $ auto                       -- ecl, minVersion, maxVersion, mask, boostEcl, result
  ) qrcodegen_encodeText
  where
    qrCodeOut = peekIncompleteArrayOut maxLen
```

If every argument is mundane, `auto` fills in with sensible default
marshallers:

```haskell
getSize :: IncompleteArray Word8 -> IO Int
getSize = toHighLevel auto qrcodegen_getSize
```

## The model

A binding is a `ToHighLevel lo hi`: a recipe that turns the low-level callable `lo`
into the high-level wrapper `hi`. It is built by chaining positions with `$` and
closing with a result converter, then run with `toHighLevel`:

| Combinator | Adds to the wrapper |
|---|---|
| `input m` | one argument, marshalled to C by `m` (`input1` / `input2` / `input3` pin the C arity) |
| `output u` | no argument; the out-parameter's value joins the result tuple |
| `scratch` / `scratchArray` / `fixed` | a hidden C argument the wrapper does not expose |
| `resultPure` / `resultIO` / `discardResult` | the closer: convert or drop the C return |
| `throwOn` / `throwOnNonZero` / `throwOnOut` | classify a status and throw |
| `dropTrailingUnit` | strip the `()` a void closer leaves beside outputs |

Each position takes a marshaller. There are three kinds, all in
`HsBindgen.HighLevel.Marshaller`:

- **`Marshal e a b`** moves one Haskell value into the leading C argument(s). Build
  one with `scalar` (a pure conversion), `bracket` (a resource held across the call),
  `at` (aim it at a field), or the ready-mades in `HsBindgen.HighLevel.Marshaller.Utils`
  (`withCStringIn`, `useAsByteStringLenIn`, `funPtrIn`, and so on).
- **`Unmarshaller c hs`** allocates an out-parameter, runs the call, and reads a
  value back (`unmarshalOut`, `peekCStringOut`, `byteStringOut`, `outForeignPtr`).
- **`MarshalStruct` / `UnmarshalStruct`** write and read a whole C struct, dropped
  into a wrapper with `asArgument`, `asOutput`, or `asResult`.

## Writing high-level bindings with typed holes

Typed holes are a good way to write high-level bindings declaratively, though
the type machinery behind them can produce confusing errors. This section
offers some advice on approaching them with `hs-bindgen-highlevel`.

The wrapper's type signature matters most: it is what GHC and instance
resolution use to infer the marshallers and to produce good typed-hole
messages.

The spec is then filled one position at a time, with a hole `_` at the current
position and the tail stubbed with `undefined`, so the spec typechecks while
GHC reports the hole.

For `parse_int :: PtrConst CChar -> Ptr CInt -> IO CInt` and a target
`hsParseInt :: String -> IO (Int, Int)`, the first C argument is one `const char *`,
so `input1` fits. Its fixed arity is type inference friendly:

```haskell
hsParseInt = toHighLevel (input1 _ $ undefined) parse_int
--   Found hole: _ :: Marshal String (PtrConst CChar -> Ptr CInt -> IO CInt) (Ptr CInt -> IO CInt)
--     Valid hole fits include withCStringIn
```

GHC suggests the marshaller, sometimes the exact one. With it, the hole moves
along, tail still `undefined`:

```haskell
hsParseInt = toHighLevel (input1 withCStringIn $ output _ $ undefined) parse_int
--   Found hole: _ :: Unmarshaller (Ptr CInt) Int
```

The output hole is concrete even with the tail stubbed, since its value type
is inferred from the declared result type. Replacing the last `undefined` with
the closer finishes the binding.

*NOTE*: `input1` / `input2` / `input3` are preferable to plain `input` while a
position is a hole. Plain `input` accepts a marshaller of any arity, which is
all a finished spec needs, but it infers poorly in a hole.

## Reading the type errors

> No default input marshaller for type …

(or output, or result). `auto`, or an explicit `defaultIn` / `defaultOut` /
`defaultRes`, encountered a type with no default instance. The fix is an explicit
marshaller for that position, or a default instance for the type:

```haskell
instance DefaultIn Handle (CInt -> lo) lo where -- newtype Handle = Handle CInt
  defaultIn = scalar (\(Handle h) -> h)
```

> auto cannot line the high-level type up with the C function

`auto` ran out of high-level arguments while C still expects one. That
leftover C argument is one the wrapper does not expose. The fix is an explicit
`output` or `scratch` for it, or the missing argument in the wrapper's signature.
`auto` fills inputs and the closer, nothing else.

**NOTE**: `auto` can _only_ be used to fill input marshallers, before or after a
non-input/result marshaller, not in between.

> This wrapper builds more than eight result components

This means the type class that handles tuple flatenning hit the hard limit of
an 8-tuple. Outputs can be combined into a struct or a `newtype`-wrapped
tuple, or a write-only one dropped with `scratch`.

> An ambiguous result type

`defaultRes` infers the result type off the signature, so a wrapper written
without a result annotation cannot resolve. Every wrapper should carry a type
signature; this is the one habit that prevents most inference troubles.

## Beyond the basics

**Errors.** Every position runs in `IO`, so a check can sit wherever the failure
signal lives, and a throw unwinds the brackets opened before it. `pcap_findalldevs`
returns a status and writes a message into a separate buffer; the buffer is passed as
`scratch` so the `resultIO` closer can read it and throw, keeping the check inside the
spec:

```haskell
findAllDevNames :: IO [String]
findAllDevNames =
  allocaBytes errbufSize $ \errbuf ->
    toHighLevel
      ( dropTrailingUnit                -- collapse the () the void closer leaves
      $ output peekPcapDeviceNames      -- pcap_if_t ** : device names (kept)
      $ scratch (\k -> k errbuf)        -- char *       : pre-allocated errbuf
      $ resultIO (throwOnStatus errbuf) -- int          : throw on failure
      ) pcap_findalldevs
```

`dropTrailingUnit` drops the `()` the void `resultIO` closer leaves beside the
output, so the wrapper returns `IO [String]` rather than `IO ([String], ())`.

`peekPcapDeviceNames` is a hand-written `Unmarshaller` (via `unmarshalOutWith`) that
walks the allocated linked list, collects the names and frees it.

**Structs.** A struct binding has two halves: a `MarshalStruct` that writes a
high-level value into the C layout, and an `UnmarshalStruct` that reads one back.
hs-bindgen emits the low-level struct (with its `StaticSize` / `ReadRaw` / `WriteRaw`
instances); the user writes the high-level type and the two marshallers.

```c
struct Point { int x; int y; };
struct Point midpoint(const struct Point *a, const struct Point *b);
```

```haskell
-- hs-bindgen emits: data Point = Point CInt CInt
data PointHi = PointHi { x :: Int, y :: Int }

-- write side: chain the fields in source order with >>>; at aims each at its marshaller
pointIn :: MarshalStruct PointHi Point
pointIn = struct Point (    at x (scalar fromIntegral)
                        >>> at y (scalar fromIntegral)
                       )

-- read side: assemble under the high-level constructor with <$> / <*>
pointOut :: UnmarshalStruct Point PointHi
pointOut = PointHi <$> unmarshalFieldPure (\(Point vx _) -> vx) fromIntegral
                   <*> unmarshalFieldPure (\(Point _ vy) -> vy) fromIntegral
```

Each half drops into a wrapper through the adapter that matches how C takes the
struct: `asArgument` / `asArgumentC` for an argument, `asOutput` for an
out-parameter, and `asResult` for a by-value return.

```haskell
midpoint :: PointHi -> PointHi -> IO PointHi
midpoint = toHighLevel
  ( input (asArgumentC pointIn) -- const struct Point *a
  $ input (asArgumentC pointIn) -- const struct Point *b
  $ asResult pointOut           -- struct Point return
  ) c_midpoint
```

The nested case, with a `(ptr, len)` string field and a nullable pointer, is in the
`$structs` section of the Haddock for `HsBindgen.HighLevel`.

**Pure calls.** For a deterministic C call (a hash, a crypto primitive) `assertPure`
exposes the `IO` wrapper as a pure function (spoiler: `unsafePerformIO`). It
asserts the call is a pure function of its inputs.

## Caveats and common mistakes

**The trailing unit.** A void closer beside an output leaves a `()` in the result:
`output a $ discardResult` has result `IO (a, ())`. Wrapping the finished spec in
`dropTrailingUnit` drops the trailing `()` to `IO a`, which avoids destructuring it
at every call site.

**Generated enums usually need no `auto` default.** When the C function takes the
enum type directly, the built-in identity `DefaultIn` instance passes it through, so
`auto` needs nothing extra. Do *not* write the identity case by hand:

```haskell
-- Don't: this overlaps the library's identity instance and fails to compile.
instance DefaultIn MyEnum (MyEnum -> lo) lo where
  defaultIn = scalar id
```

An instance is only needed when the C function takes a *different* type than the
Haskell enum, for example a plain `CInt`, and it must marshal to that type:

```haskell
instance DefaultIn MyEnum (CInt -> lo) lo where
  defaultIn = scalar enumToCInt -- the MyEnum -> CInt conversion
```

**Silent numeric conversion.** The scalar defaults convert with `fromIntegral` and
`realToFrac`, which are lossy across widths and signedness. Where precision matters,
an explicit marshaller should replace `defaultIn`.

**Unchecked field projections.** `at` and `unmarshalField` take a projection whose
correctness beyond its type is unchecked: a wrong but same-typed field still
compiles, so field and projection must be matched with care.

**Call-scoped callbacks.** `funPtrIn` frees the `FunPtr` when the call returns, so it
is for a callback the C function invokes during the call, not one it stores.

## Status

Pre-release (alpha), developed alongside hs-bindgen. The API is still settling.
Owned by Well-Typed LLP and Anduril Industries; BSD-3-Clause.
