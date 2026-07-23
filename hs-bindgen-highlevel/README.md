# `hs-bindgen-highlevel`

Write high-level Haskell wrappers over low-level FFI bindings, declaratively.

Wrapping a C function by hand is repetitive: allocate a buffer, pass a `Ptr`,
thread an out-parameter, read it back, check a status code, free whatever C
handed back. This library gives you combinators for saying what each C argument
is for, and does the marshalling from that description. A binding is a
value you build and then run:

```c
strcmp(const char * str1, const char * str2, size_t n);
```

```hs
c_strncmp :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt

hsStrncmp :: String -> ByteString -> IO Int
hsStrncmp = toHighLevel c_strncmp
          $ input  withCStringIn        -- fills 1 C argument
          $ input2 useAsByteStringLenIn -- fills 2 C arguments
          $ resultPure fromIntegral
```

That reads top to bottom like an annotated C prototype: one `input` per Haskell
argument, marshalled into the C argument or arguments it fills, closed by a
conversion of the return value.

The approach is inspired by [c2hs][], but
with no custom syntax and no code generation step. A spec is an ordinary
Haskell value, so you can name it, reuse it, and let the type checker check
it.

## Quick start

The wrappers below come from [`examples/libsodium`][example:libsodium], each
preceded by the C it wraps. The first two are written out combinator by
combinator. The rest show how much of that writing out you can hand back to the
library.

### Writing a spec by hand

**Inputs and results.** One combinator per C argument, top to bottom,
closed by an explicit combinator to deal with the return value. `defaultIn`
picks a default marshaller according to the input type, `input2` fills the
`(m, mlen)` pair from one `ByteString`, and `resultPure` turns the status into
a `Bool`:

```c
int crypto_sign_verify_detached(const unsigned char *sig, const unsigned char *m,
                                unsigned long long mlen, const unsigned char *pk);
```

```haskell
verifyDetached :: PublicKey -> Signature -> ByteString -> Bool
verifyDetached publicKey signature message = toHighLevelPure crypto_sign_verify_detached
  ( input  defaultIn             -- const unsigned char *sig
  $ input2 unsafeByteStringLenIn -- m, mlen
  $ input  defaultIn             -- const unsigned char *pk
  $ resultPure (== 0)            -- status -> Bool
  ) signature message publicKey
```

Note the trailing arguments. They are applied in the order the spec consumes
them, which is C's order, not the wrapper's.

Note also `toHighLevelPure` rather than `toHighLevel`. It runs the spec the same
way and then takes the `IO` off the result (via `unsafePerformIO`), which you
may do whenever the call is a function of its arguments; Ed25519 verification
is one.

**NOTE**: Every combinator here is a default, so this whole spec can just be
`autoWith (== 0)`. More on `auto` below.

**Out-parameters.** `crypto_sign_detached` delivers the signature by writing into
two caller-allocated slots, and reports success in its return value. Each
`output` allocates a slot and reads it back after the call. The closer's
function receives every output in spec order and then the C return value, so
`takeSignature` here is a `ByteString -> Int -> CInt -> IO Signature`:

```c
int crypto_sign_detached(unsigned char *sig, unsigned long long *siglen_p,
                         const unsigned char *m, unsigned long long mlen,
                         const unsigned char *sk);
```

```haskell
signDetached :: SecretKey -> ByteString -> Signature
signDetached secretKey message = toHighLevelPure crypto_sign_detached
  ( output (byteStringOut signatureBytes)  -- unsigned char *sig
  $ output (unmarshalOutPure fromIntegral) -- unsigned long long *siglen_p
  $ input2 unsafeByteStringLenIn           -- m, mlen
  $ input  defaultIn                       -- const unsigned char *sk
  $ resultIO (takeSignature "crypto_sign_detached")
  ) message secretKey
```

`input`, `output` and `resultIO` are the whole vocabulary: a combinator per C
argument, and a closer. Everything else follows from them.

### Letting `auto` write the ordinary combinators

Most combinators in most bindings don't require a decision, and `auto` fills
those off the high-level type signature. Where nothing needs a decision it is
the entire spec:

```c
int sodium_library_version_major(void);
```

```haskell
major :: IO Int
major = toHighLevel sodium_library_version_major auto
```

That is the whole binding. `auto` filled the arguments, of which there are none,
and converted the `int` to `Int` because the signature asks for `Int`.

### Keeping the half you care about

`auto` runs to the end of the spec once it starts, so on its own it is all or
nothing. Two variants give back one half each.

**`autoResult` fills the result only,** leaving the combinators to you. Here a
single `output` is the entire result, so there is nothing left to decide about
it:

```c
void crypto_secretbox_keygen(unsigned char k[32]);
```

```haskell
newKey :: IO Key
newKey = toHighLevel crypto_secretbox_keygen
       $ output (Key <$> byteStringOut keyBytes)
       $ autoResult
```

**`autoInputs` fills the inputs only,** then hands over to a spec you wrote. It
composes with any closer, for example:

- `autoChecked` is `autoInputs` closed by a status check that throws
- `autoMaybe` is `autoInputs` closed by a status check that gives `Nothing`
- `autoWith` is `autoInputs` closed by a result function.

### Mixing hand-written and automatic combinators

A real binding is usually not something one can one-shot with `auto`.
`qrcodegen_encodeText` is an example: a scratch buffer the caller allocates
but never reads, an out-parameter holding the QR code, and five ordinary
arguments that deserve no thought at all:

```c
bool qrcodegen_encodeText(const char *text, uint8_t tempBuffer[], uint8_t qrcode[],
                          enum qrcodegen_Ecc ecl, int minVersion, int maxVersion,
                          enum qrcodegen_Mask mask, bool boostEcl);
```

Write out only the combinators that need a decision. `scratchArray` allocates the temp
buffer and keeps it out of the type signature, `output` keeps the QR code, and
`auto` fills everything else from the signature, the result included:

```haskell
encodeText
  :: String -> Qrcodegen_Ecc -> Int -> Int -> Qrcodegen_Mask -> Bool
  -> IO (IncompleteArray Word8, Bool)
encodeText = toHighLevel qrcodegen_encodeText
           $ input defaultIn     -- text (String)
           $ scratchArray maxLen -- tempBuffer: written, never read
           $ output qrCodeOut    -- qrcode: the out-parameter we keep
           $ auto                -- ecl, minVersion, maxVersion, mask, boostEcl,
                                 -- then the (qrcode, ok) result
  where
    qrCodeOut = peekIncompleteArrayOut maxLen
```

`auto` runs to the end of the spec once it starts, which is why the leading
`text` argument is written out as `input defaultIn` rather than left to it: an
ordinary input sitting above an explicit combinator costs one line to say so.

## The combinators

A binding is a `ToHighLevel os lo hi`: a recipe that turns the low-level
type signature `lo` into the high-level type signature `hi`, collecting the
out-parameter types `os` along the way. Chain combinators with `$`, finish the
spec with a closer combinator, run with `toHighLevel`. When the C call really
is a function of its inputs, `toHighLevelPure` runs it the same way and hands
back the wrapper with the `IO` taken off.

| Combinator | Adds to the high-level type |
|---|---|
| `input m` | one argument, marshalled by `m` into one C argument (`input2` / `input3` / `inputN` for other arities) |
| `output u` | no argument; the out-parameter's value becomes an argument to the closer |
| `scratch` / `scratchArray` / `fixed` | a C argument the high-level type does not expose |
| `resultPure` / `resultiO` | the closer: build the result from the outputs and the C return value |
| `discardResult` / `throwOnNonZero` / `asResult` | closers for specs with no outputs |
| `auto` / `autoResult` | fill the remaining inputs, and the result, from the signature |
| `autoInputs` | fill the remaining inputs only, then continue with a spec you wrote |
| `checkedResult` / `maybeResult` / `eitherResult` | closers for a C return value that is a status guarding the outputs, which throw, give `Nothing`, or give `Left e` |
| `autoWith` / `autoChecked` / `autoMaybe` | `autoInputs` plus `resultPure` / `checkedResult` / `maybeResult` |
| `throwOn` / `throwOnOut` | classify a status and throw |

Each combinator takes a marshaller, of which there are three kinds:

- **`Marshal hs lo lo'`** moves one Haskell value into the leading C
  argument(s). Build one with `scalar` for a pure conversion, `bracket` for a
  resource held across the call, `at` to aim one at a struct field, or take a
  ready-made one from `HsBindgen.HighLevel.Marshaller.Utils` (`withCStringIn`,
  `useAsByteStringLenIn`, `funPtrIn`).
- **`Unmarshaller c hs`** allocates an out-parameter slot and reads the filled
  slot back (`unmarshalOut`, `peekCStringOut`, `byteStringOut`,
  `outForeignPtr`).
- **`MarshalStruct` / `UnmarshalStruct`** write and read a whole C struct, and
  use in a spec with `asargument`, `asOutput`, or `asResult`.

## Writing your own combinators

A C library rarely has a hundred unrelated functions. Usually it has one or two
conventions repeated a lot of times: every constructor fills an out-parameter
and returns a status, every accessor hands back a borrowed pointer, every
getter fills a caller-sized buffer. Writing high-level bindings manually, is
tiresome.

Since a spec is an ordinary Haskell value, it is possible to abstract over a
convention using ordinary Haskell.

### Ordinary Haskell first: libgit2's handle constructors

Every libgit2 constructor has the same three-part shape. Three of them, with the
free function each of their results needs:

```c
int  git_repository_open (git_repository **out, const char *path);
int  git_commit_lookup   (git_commit **out, git_repository *repo, const git_oid *id);
int  git_revwalk_new     (git_revwalk **out, git_repository *repo);

void git_repository_free (git_repository *repo);
void git_commit_free     (git_commit *commit);
void git_revwalk_free    (git_revwalk *walk);
```

A `git_X **out` slot to fill, then some inputs, then a status: `0` for success, a
negative code for failure, with the message left in thread-local state. Only the
middle part differs, and only in how many arguments it has.

**The Haskell side.** A `git_X *` is opaque and has to be freed exactly once, so
each becomes a newtype over a `ForeignPtr` carrying its own `git_X_free` as
finaliser. Freeing then happens at GC and no caller ever calls it:

```haskell
newtype Repository = Repository (ForeignPtr Git_repository)
newtype Commit     = Commit     (ForeignPtr Git_commit)
newtype Revwalk    = Revwalk    (ForeignPtr Git_revwalk)
```

libgit2 has ten of those, all with the same two operations, so one class collapses
them. `CRep h` is the generated C type a handle wraps, and it is injective because
no two handles wrap the same one:

```haskell
class Handle h where
  type CRep h = r | r -> h          -- CRep Commit = Git_commit, and back again
  toFP   :: h -> ForeignPtr (CRep h)
  fromFP :: ForeignPtr (CRep h) -> h

instance Handle Commit where
  type CRep Commit = Git_commit
  toFP (Commit p)  = p
  fromFP           = Commit
```

Two marshallers follow from it directly. One passes a handle to C, holding the
`ForeignPtr` alive across the call so the finaliser cannot fire mid-call; the
other fills a `git_X **` slot and attaches the finaliser to whatever C wrote
there:

```haskell
handleIn  :: Handle h => Marshal h (Ptr (CRep h) -> lo) lo
handleIn  = bracket (\h -> withForeignPtr (toFP h))

outHandle :: Handle h => FinalizerPtr (CRep h) -> Unmarshaller (Ptr (Ptr (CRep h))) h
outHandle = fmap fromFP . outForeignPtr
```

The status convention is worth naming once too, since every fallible call in the
library shares it:

```haskell
checkStatus :: CInt -> IO ()
checkStatus n | n < 0     = throwIO =<< gitError n -- reads git_error_last
              | otherwise = pure ()

checkedStatus :: AutoOutputs os hs => ToHighLevel os (IO CInt) (IO hs)
checkedStatus = checkedResult checkStatus
```

**The repetition.** With those in hand, the three constructors are one spec
written three times:

```haskell
repositoryOpen :: Text -> IO Repository
repositoryOpen = toHighLevel git_repository_open
               $ output (outHandle git_repository_free)
               $ input textIn
               $ checkedStatus

commitLookup :: Repository -> Oid -> IO Commit
commitLookup = toHighLevel git_commit_lookup
             $ output (outHandle git_commit_free)
             $ input handleIn
             $ input oidInC
             $ checkedStatus

revwalkNew :: Repository -> IO Revwalk
revwalkNew = toHighLevel git_revwalk_new
           $ output (outHandle git_revwalk_free)
           $ input handleIn
           $ checkedStatus
```

Three of those four lines are the same shape every time: `toHighLevel` on the C
function, one `output (outHandle ...)` for the handle it produces, and
`checkedStatus` at the bottom. Only the inputs in between vary, in number and in
kind.

**The abstraction.** So take the middle as an argument. It is already a function:
`input textIn` and `input handleIn . input oidInC` both take the rest of the spec
and give back the chain with that rest attached. Name it, and the top and the
bottom get written once:

```haskell
newHandle
  :: (Handle h, ThreadIn hi)
  => FinalizerPtr (CRep h)                                         -- git_X_free
  -> (ToHighLevel '[h] (IO CInt) (IO h) -> ToHighLevel '[h] lo hi) -- the caller's inputs
  -> (Ptr (Ptr (CRep h)) -> lo)                                    -- the C function
  -> hi
newHandle fin inputs = flip toHighLevel
                     $ output (outHandle fin)
                     $ inputs checkedStatus
```

Three things in that type read better slowly:

- `'[h]` is the spec's output list. This spec collects exactly one out-parameter,
  the handle, and it is still sitting there when `checkedStatus` collects it. The
  caller's slice sits inside that, which is why `'[h]` appears at both of its ends.
- The second argument's type is "a spec from the closer downwards, given the
  closer", which is exactly what a chain of `input`s is.
- `flip toHighLevel`, because `toHighLevel` wants the C function first and here it
  arrives last, as the argument that makes `newHandle` applicable to a raw
  `foreign import`.

Each constructor is now one line, whatever its arity:

```haskell
repositoryOpen :: Text -> IO Repository
repositoryOpen = newHandle git_repository_free (input textIn) git_repository_open

commitLookup :: Repository -> Oid -> IO Commit
commitLookup = newHandle git_commit_free (input handleIn . input oidInC) git_commit_lookup

revwalkNew :: Repository -> IO Revwalk
revwalkNew = newHandle git_revwalk_free (input handleIn) git_revwalk_new
```

libgit2's ten handle types and every one of their constructors go through that
single definition. The accessors go the same way, and there the varying part is
the C function alone, so it is the only argument:

```c
const char    *git_commit_message(const git_commit *commit);
const git_oid *git_commit_id     (const git_commit *commit);
```

```haskell
borrowedText :: Handle h => (PtrConst (CRep h) -> IO (PtrConst CChar)) -> (h -> IO Text)
borrowedText = flip toHighLevel
             $ input handleInC
             $ resultIO peekTextConst

commitMessage :: Commit -> IO Text
commitMessage = borrowedText git_commit_message
```

### When the shape itself varies: the `auto` constraints

`newHandle` gets away with plain parametrisation because it knows the shape of the
specs it builds: always one out-parameter, always closed by `checkedStatus`. Only
the inputs vary, and they vary as a value it can take.

A combinator that has to work at *any* number of arguments, or *any* number of
out-parameters, or *any* C return type, cannot take those as values. It has to say
in its context that they line up, and four constraints cover that.

| Constraint | Says |
|---|---|
| `Auto os lo hi` | the rest of the spec writes itself |
| `AutoInputs lo hi lo' hi'` | filling every argument of `hi` from `lo` leaves `lo'` and `hi'` as the rest |
| `AutoOutputs os hs` | the out-parameters alone assemble into `hs` |
| `AutoResult os c hs` | the out-parameters and the C return value assemble into `hs` |

Read `AutoInputs` in pairs: `lo` and `hi` are the two types going in, `lo'` and
`hi'` the two coming out.

### Let the compiler write the signature

You do not have to work those constraints out. Write the body and the *result*
type, leave the context off, and the error tells you what to add. Start with what
you know: this combinator collects no out-parameters, so `os` is `'[]`, and the
rest is open.

```haskell
-- foo abstracts over the common pattern of C library functions where there's
-- only input parameters but the return status code needs to be checked.
foo :: ToHighLevel '[] lo hi
foo = autoInputs $ resultPure (== 0)
```

```
• No instance for ‘AutoInputs lo hi (IO Integer) (IO Bool)’
    arising from a use of ‘autoInputs’
```

That is the constraint, spelled out. Paste it back and the module compiles:

```haskell
foo :: AutoInputs lo hi (IO Integer) (IO Bool) => ToHighLevel '[] lo hi
```

One thing to fix by hand. `Integer` is not something the library chose; it is
where GHC defaulted the `0` in `(== 0)`, and no C function returns one. Replace
it with the C type you meant, or leave it open and let the call site decide:

```haskell
foo :: ( AutoInputs lo hi (IO c) (IO Bool)
       , Eq c
       , Num c
       )
    => ToHighLevel '[] lo hi
foo = autoInputs $ resultPure (== 0)
```

This signature reads as: `foo` returns any specification that converts `lo`
into `hi`, where all parameters are input parameters _and_ where the return
type of `lo` is `IO c` (where `Eq c`, `Num c` hold) and the return type of
`hi` is `IO Bool` (due to `(== 0)`).

### Following it further

A combinator in the body asks for the constraint of the same name in the
context.

| The body uses | The context needs |
|---|---|
| `autoInputs` | `AutoInputs lo hi lo' hi'` |
| `auto` | `Auto os lo hi` |
| `autoResult` | `AutoResult os c hs` |
| `autoOutputs`, or a closer built on it (`checkedResult` / `maybeResult` / `eitherResult`) | `AutoOutputs os hs` |
| `resultPure` / `resultIO` over an assembler the caller passes in | `ApplyOutputs os` |

## What a spec compiles to

Every combinator carries an `INLINE` pragma, and at a finished binding both
type indices are concrete, so every class method resolves to a known instance;
from `-O1` on, what is left is the `alloca` / call / `peek` code you would
have written by hand. Put a spec and a hand-written wrapper in one module and
`ghc -O -ddump-simpl` shows them as the same binding.

Two mechanisms account for every derivation below, and there are only two.

**Brackets are deferred.** `threadIn` pushes the bracket past the arguments
that are still to come.

```haskell
threadIn :: forall a. (forall r. (a -> IO r) -> IO r) -> (a -> hi) -> hi

instance ThreadIn (IO r) where
  threadIn br f = br f                                  -- open the bracket
instance ThreadIn rest => ThreadIn (arg -> rest) where
  threadIn br f = \arg -> threadIn br (\a -> f a arg)   -- peel an argument, recurse
```

### Inputs: where the brackets open

```c
int strncmp(const char *s1, const char *s2, size_t n);
```

```haskell
c_strncmp :: PtrConst CChar -> PtrConst CChar -> CSize -> IO CInt

hsStrncmp :: String -> ByteString -> IO Int
hsStrncmp = toHighLevel c_strncmp
          $ input  withCStringIn
          $ input2 useAsByteStringLenIn
          $ resultPure fromIntegral
```

compiles to

```haskell
hsStrncmp s bs =
  withCString s $ \p1 ->
    BS.useAsCStringLen bs $ \(p2, n) ->
      fromIntegral <$> c_strncmp (unsafeFromPtr p1) (unsafeFromPtr (castPtr p2))
                                 (fromIntegral n)
```

No out-parameters, so `os` stays `'[]` throughout, `AssembleOutputs '[] (CInt -> Int)`
reduces to `CInt -> Int`, and `fromIntegral` is used at exactly that type. Nothing
here was inferred from a default.

### Out-parameters: where the types come from

```c
void two_out(int *a, int *b);
```

```haskell
c_twoOut :: Ptr CInt -> Ptr CInt -> IO ()

hsTwoOut :: IO (Int, Int)
hsTwoOut = toHighLevel c_twoOut
         $ output defaultOut
         $ output defaultOut
         $ autoResult
```

compiles to

```haskell
hsTwoOut =
  alloca $ \pa ->
    alloca $ \pb -> do
      c_twoOut pa pb
      a <- fromIntegral <$> peek pa
      b <- fromIntegral <$> peek pb
      pure (a, b)
```

### Everything at once

```c
int render(int *out, const char *name, char *scratch, int flags);
```

```haskell
hsRender :: String -> IO Int
hsRender = toHighLevel c_render
         $ output defaultOut           -- int *out
         $ input  withCStringIn        -- const char *name
         $ scratchArray @CChar 16      -- char *scratch
         $ fixed  0                    -- int flags
         $ checkedResult rejectNonZero
```

compiles to

```haskell
hsRender name =
  alloca $ \out ->
    withCString name $ \cname ->
      allocaArray 16 $ \scratch -> do
        status <- c_render out (unsafeFromPtr cname) scratch 0
        rejectNonZero status
        fromIntegral <$> peek out
```

Two things, both consequences of the rules above.

## Caveats

**`auto` closes the spec, and `autoInputs` takes every argument that is left.**
Neither can be told to fill a chosen number of arguments, so both come last:
`auto` last in the spec, `autoInputs` last among the combinators that consume an
argument. For a single argument, `input defaultIn` is the same step done once,
and it mixes with hand-written combinators anywhere.

**`auto` assembles positionally, so it cannot tell same-typed components apart.**
For `void get_range(int *value, int *status)` and `getRange :: IO (Int, Int)` it
builds `(value, status)`. Someone who meant `(status, value)` gets a type that
compiles, runs, and is wrong, because both components are `Int`. Where result
components share a type and the C signature does not settle their order, write
the closer out, which names each one:

```haskell
   output valueOut
 $ output statusOut
 $ resultPure (\value status _ -> (status, value))
```

**Numeric conversion is silent.** The scalar defaults convert with `fromIntegral`
and `realToFrac`, which are lossy across widths and signedness. Where precision
matters, replace `defaultIn` with an explicit marshaller.

**The string result defaults borrow.** A binding that says `IO String` over a C
function returning `const char *` gets `DefaultRes`, which copies the bytes out
and frees nothing, because the usual C function of that shape keeps owning the
memory. A call that instead expects the caller to free needs a closer that frees.
Nothing in the type distinguishes the two.

**Callbacks are call-scoped.** `funPtrIn` frees the `FunPtr` once the call
returns, so it fits a callback C invokes during the call, not one C stores.

## Documentation

The Haddock for `HsBindgen.HighLevel` covers the rest:

- *Reading the signatures*, for what the three type indices mean and why a spec
  infers. A mistake in a spec surfaces as a type error against these.
- *Building against typed holes*, the intended way to write a spec: signature
  first, one hole at a time, `auto` as the tail stub.
- *Reading the type errors*, for the ones that come up most.
- *A worked struct example*, covering nesting, a `(ptr, len)` string field, and a
  nullable pointer.
- *Writing a combinator of your own*, which walks through `output` line by line.

`HsBindgen.HighLevel.Auto` documents what `auto` fills, what it leaves alone, and
the four constraints a combinator of your own is written against.
`HsBindgen.HighLevel.Unlifted` covers by-value structs, which GHC's FFI cannot
pass directly.

Three examples ship a low-level and a high-level version of the same program,
which print identical output:

- [`libgit2`][example:libgit2] for a handle-based API whose error detail lives in
  thread-local state
- [`libsodium`][example:libsodium] for a buffer-oriented API with
  caller-allocated output
- [`libclang-ffi`][example:libclang-ffi] for by-value structs

[`c-qrcode`][example:c-qrcode] and [`libpcap`][example:libpcap] have smaller
wrappers without a low-level counterpart.

## Status

Pre-release (alpha). The API is still settling.

Owned by Well-Typed LLP and Anduril Industries. BSD-3-Clause.

<!-- sources and references -->

[c2hs]: https://github.com/haskell/c2hs
[example:c-qrcode]: https://github.com/well-typed/hs-bindgen/tree/main/examples/c-qrcode
[example:libclang-ffi]: https://github.com/well-typed/hs-bindgen/tree/main/examples/libclang-ffi
[example:libgit2]: https://github.com/well-typed/hs-bindgen/tree/main/examples/libgit2
[example:libpcap]: https://github.com/well-typed/hs-bindgen/tree/main/examples/libpcap
[example:libsodium]: https://github.com/well-typed/hs-bindgen/tree/main/examples/libsodium
