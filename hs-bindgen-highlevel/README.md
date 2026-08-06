# `hs-bindgen-highlevel`

Write high-level Haskell wrappers over low-level FFI bindings, declaratively.

Wrapping a C function by hand is repetitive: allocate a buffer, pass a `Ptr`,
thread an out-parameter, read it back, check a status code, free whatever C
handed back. This library gives you combinators for saying what each C argument
is for, and does the marshalling from that description. A binding is a
value you build and then run:

```c
c_strcmp(const char * str1, const char * str2, size_t n);
```

```hs
hsStrncmp :: String -> ByteString -> IO Int
hsStrncmp = toHighLevel ( input  withCStringIn        -- fills 1 C argument
                        $ input2 useAsByteStringLenIn -- fills 2 C arguments
                        $ resultPure fromIntegral
                        ) c_strncmp
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
preceded by the C it wraps. The first two are written out position by
position. The rest show how much of that writing out you can hand back to the
library.

### Writing a spec by hand

**Inputs and a converted result.** One position per C argument, top to bottom,
closed by a conversion of the return value. `defaultIn` picks a default
marshaller the key types, `input2` fills the `(m, mlen)` pair from one
`ByteString`, and `resultPure` turns the status into a `Bool`:

```c
int crypto_sign_verify_detached(const unsigned char *sig, const unsigned char *m,
                                unsigned long long mlen, const unsigned char *pk);
```

```haskell
verifyDetached :: PublicKey -> Signature -> ByteString -> IO Bool
verifyDetached publicKey signature message = toHighLevel
  ( input  defaultIn             -- const unsigned char *sig
  $ input2 unsafeByteStringLenIn -- m, mlen
  $ input  defaultIn             -- const unsigned char *pk
  $ resultPure (== 0)            -- status -> Bool
  ) crypto_sign_verify_detached signature message publicKey
```

Note the trailing arguments. They are applied in the order the spec consumes
them, which is C's order, not the wrapper's.

**Out-parameters.** `crypto_sign_detached` delivers the signature by writing into
two caller-allocated slots, and reports success in its return value. Each
`output` allocates a slot and reads it back after the call. The closer's function
receives every output in spec order and then the C return value, so
`takeSignature` here is a `ByteString -> Int -> CInt -> IO Signature`:

```c
int crypto_sign_detached(unsigned char *sig, unsigned long long *siglen_p,
                         const unsigned char *m, unsigned long long mlen,
                         const unsigned char *sk);
```

```haskell
signDetached :: SecretKey -> ByteString -> IO Signature
signDetached secretKey message = toHighLevel
  ( output (byteStringOut signatureBytes)  -- unsigned char *sig
  $ output (unmarshalOutPure fromIntegral) -- unsigned long long *siglen_p
  $ input2 unsafeByteStringLenIn           -- m, mlen
  $ input  defaultIn                       -- const unsigned char *sk
  $ resultIO (takeSignature "crypto_sign_detached")
  ) crypto_sign_detached message secretKey
```

Those two are the whole vocabulary: a position per C argument, and a closer.
Everything below is the same thing with less typing.

### Letting `auto` fill the ordinary positions

Most positions in most bindings hold no decision, and `auto` reads those off the
wrapper's type signature. Where nothing needs a decision it is the entire spec:

```c
int sodium_library_version_major(void);
```

```haskell
major :: IO Int
major = toHighLevel auto sodium_library_version_major
```

That is the whole binding. `auto` filled the arguments, of which there are none,
and converted the `int` to `Int` because the signature asks for `Int`.

### Keeping the half you care about

`auto` runs to the end of the spec once it starts, so on its own it is all or
nothing. Two variants give back one half each.

**`autoResult` fills the result only,** leaving the positions to you. Here a
single `output` is the entire result, so there is nothing left to decide about
it:

```c
void crypto_secretbox_keygen(unsigned char k[32]);
```

```haskell
newKey :: IO Key
newKey = toHighLevel
  ( output (Key <$> byteStringOut keyBytes) -- unsigned char k[32]
  $ autoResult
  ) crypto_secretbox_keygen
```

**`autoInputs` fills the inputs only,** then hands over to a spec you wrote. Its
two common pairings have their own names, and the next two examples are those:
`autoChecked` is `autoInputs` closed by a status check, `autoWith` is
`autoInputs` closed by a result function.

**A status that guards the outputs.** `crypto_secretbox_easy` writes the
ciphertext into `c` and reports success in its return value. `autoChecked` fills
the two ordinary arguments left (`n` and `k`) and then closes on the status:

```c
int crypto_secretbox_easy(unsigned char *c, const unsigned char *m,
                          unsigned long long mlen, const unsigned char *n,
                          const unsigned char *k);
```

```haskell
encrypt :: Key -> Nonce -> ByteString -> IO ByteString
encrypt key nonce message = toHighLevel
  ( output (byteStringOut (macBytes + BS.length message)) -- unsigned char *c
  $ input2 unsafeByteStringLenIn                          -- m, mlen
  $ autoChecked (checkStatus "crypto_secretbox_easy")     -- n, k
  ) crypto_secretbox_easy message nonce key
```

The check runs *before* the out-parameter is read back, so a rejected call never
peeks a slot C did not write.

**A status that decides the result.** `crypto_secretbox_open_easy` has the same
shape, and the only difference is what its status means. A forged ciphertext is
expected input rather than a failure, so the status picks between `Just` and
`Nothing` instead of throwing, and `autoWith` closes with a function of your own
where `autoChecked` closed with a check:

```c
int crypto_secretbox_open_easy(unsigned char *m, const unsigned char *c,
                               unsigned long long clen, const unsigned char *n,
                               const unsigned char *k);
```

```haskell
open :: Key -> Nonce -> ByteString -> IO (Maybe ByteString)
open key nonce ciphertext
  | BS.length ciphertext < macBytes = pure Nothing -- too short to carry a MAC
  | otherwise = toHighLevel
      ( output (byteStringOut (BS.length ciphertext - macBytes)) -- unsigned char *m
      $ input2 unsafeByteStringLenIn                             -- c, clen
      $ autoWith (\plaintext status ->                           -- n, k
          if status == 0 then Just plaintext else Nothing)
      ) crypto_secretbox_open_easy ciphertext nonce key
```

That pair is the distinction worth remembering: reach for `autoChecked` when a
non-zero status means the call failed, and `autoWith` when the status is part of
the answer.

### Mixing hand-written and automatic positions

A real binding is usually neither all one nor all the other, and a spec takes
both. `qrcodegen_encodeText` is the case: a scratch buffer the caller allocates
but never reads, an out-parameter holding the QR code, and five ordinary
arguments that deserve no thought at all:

```c
bool qrcodegen_encodeText(const char *text, uint8_t tempBuffer[], uint8_t qrcode[],
                          enum qrcodegen_Ecc ecl, int minVersion, int maxVersion,
                          enum qrcodegen_Mask mask, bool boostEcl);
```

Name only the positions that need a decision. `scratchArray` allocates the temp
buffer and keeps it out of the type signature, `output` keeps the QR code, and
`auto` fills everything else from the signature, the result included:

```haskell
encodeText
  :: String -> Qrcodegen_Ecc -> Int -> Int -> Qrcodegen_Mask -> Bool
  -> IO (IncompleteArray Word8, Bool)
encodeText = toHighLevel
  ( input defaultIn      -- text (String)
  $ scratchArray maxLen  -- tempBuffer: written, never read
  $ output qrCodeOut     -- qrcode: the out-parameter we keep
  $ auto                 -- ecl, minVersion, maxVersion, mask, boostEcl,
  ) qrcodegen_encodeText --   then the (qrcode, ok) result
  where
    qrCodeOut = peekIncompleteArrayOut maxLen
```

`auto` runs to the end of the spec once it starts, which is why the leading
`text` argument is written out as `input defaultIn` rather than left to it: an
ordinary input sitting above an explicit position costs one line to say so.

## The combinators

A binding is a `ToHighLevel os lo hi`: a recipe that turns the low-level
callable `lo` into the high-level wrapper `hi`, collecting the out-parameter
types `os` along the way. Chain positions with `$`, close with a result
converter, run with `toHighLevel`. Where the C call really is a function of its
inputs, `toHighLevelPure` runs it the same way and hands back the wrapper with
the `IO` taken off, so `hsHypot :: Double -> Double -> Double` is a signature you
can write.

| Combinator | Adds to the wrapper |
|---|---|
| `input m` | one argument, marshalled by `m` into one C argument (`input2` / `input3` / `inputN` for other arities) |
| `output u` | no argument; the out-parameter's value becomes an argument to the closer |
| `scratch` / `scratchArray` / `fixed` | a C argument the wrapper does not expose |
| `resultPure` / `resultIO` | the closer: build the result from the outputs and the C return value |
| `discardResult` / `throwOnNonZero` / `asResult` | closers for specs with no outputs |
| `auto` / `autoResult` | fill the remaining inputs, and the result, from the signature |
| `autoInputs` | fill the remaining inputs only, then continue with a spec you wrote |
| `checkedResult` | the closer for a C return value that is a status guarding the outputs |
| `autoWith` / `autoChecked` | `autoInputs` plus `resultPure` / `checkedResult` |
| `throwOn` / `throwOnOut` | classify a status and throw |

Each position takes a marshaller, of which there are three kinds:

- **`Marshal hs lo lo'`** moves one Haskell value into the leading C
  argument(s). Build one with `scalar` for a pure conversion, `bracket` for a
  resource held across the call, `at` to aim one at a struct field, or take a
  ready-made one from `HsBindgen.HighLevel.Marshaller.Utils` (`withCStringIn`,
  `useAsByteStringLenIn`, `funPtrIn`).
- **`Unmarshaller c hs`** allocates an out-parameter slot and reads the filled
  slot back (`unmarshalOut`, `peekCStringOut`, `byteStringOut`,
  `outForeignPtr`).
- **`MarshalStruct` / `UnmarshalStruct`** write and read a whole C struct, and
  drop into a wrapper with `asArgument`, `asOutput`, or `asResult`.

`HsBindgen.HighLevel.Defaults` gives each Haskell type a default marshaller, and
`HsBindgen.HighLevel.Auto` uses those to fill the ordinary positions.

## Caveats

**`auto` closes the spec, and `autoInputs` takes every argument that is left.**
Neither can be told to fill a chosen number of positions, so both come last:
`auto` last in the spec, `autoInputs` last among the positions that consume a
wrapper argument. For a single position, `input defaultIn` is the same step done
once, and it mixes with hand-written positions anywhere.

**`auto` assembles positionally, so it cannot tell same-typed components apart.**
For `void get_range(int *value, int *status)` and `getRange :: IO (Int, Int)` it
builds `(value, status)`. Someone who meant `(status, value)` gets a wrapper that
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

`HsBindgen.HighLevel.Auto` documents what `auto` fills and what it leaves alone.
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
