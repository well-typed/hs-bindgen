-- | The default marshaller for each Haskell type, so that the ordinary positions in
-- a spec need no hand-written one.
--
-- One class per position: 'DefaultIn' for an argument, 'DefaultOut' for an
-- out-parameter, 'DefaultRes' for the return value. Use them as @input defaultIn@,
-- @output defaultOut@ and @defaultRes@, or let "HsBindgen.HighLevel.Auto" fill them
-- in for you.
--
-- The Haskell type in the wrapper's signature picks the representation, the same way
-- in all three positions. The idiomatic scalars convert (@Int@ and @CInt@, @Bool@ and
-- @CBool@, @Double@ and @CDouble@, and so on) and everything else passes through
-- unchanged, so both of these compile and they mean different things:
--
-- > hsGetSize :: IncompleteArray Word8 -> IO Int    -- the int is converted to Int
-- > hsGetSize :: IncompleteArray Word8 -> IO CInt   -- the int is kept as it is
--
-- 'String', 'ByteString' and 'IncompleteArray' have input defaults too. They have no
-- /output/ default, because reading a buffer back needs a length and a length is not
-- part of a type.
--
-- To give one of your own types a default, write one instance:
--
-- > newtype Handle = Handle CInt
-- > instance DefaultIn Handle (CInt -> lo) lo where
-- >   defaultIn = scalar (\(Handle h) -> h)
--
-- The instances here are a policy, not a mechanism: for a different representation,
-- pass an explicit marshaller and do not import this module.
--
-- __Warning: silent numeric conversion.__ The scalar defaults convert with
-- 'fromIntegral' \/ 'realToFrac', which are lossy and silent: @Int -> CInt@ truncates
-- a 64-bit 'Int' to 32 bits, and @CSize -> Int@ can wrap a large @size_t@ negative.
-- Where a value can fall outside the target range, keep the C type (which passes
-- through unchanged) or write a checked marshaller.
--
-- __Warning: the buffer defaults copy.__ 'String' and 'ByteString' resolve to
-- @withCStringIn@ and @constByteStringLenIn@, both of which copy the payload and
-- append a NUL. That is the safe default, and for a C function taking a
-- NUL-terminated string it is the only correct one. But where C takes an explicit
-- length the NUL buys nothing and the copy may be worth avoiding, on a large payload
-- or on secret material you would rather not duplicate. Name
-- @unsafeByteStringLenIn@ at those positions, which passes a pointer into the
-- 'ByteString' itself; it is @unsafe@ only in that C must not retain the pointer past
-- the call. Reaching for @auto@ picks the copying default, so a spec that needs the
-- other one has to say so.
--
module HsBindgen.HighLevel.Defaults (
    DefaultIn (..)
  , DefaultOut (..)
  , DefaultRes (..)
  , defaultRes
  ) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CBool, CChar, CDouble, CFloat, CInt, CSize, CUInt)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable)
import GHC.TypeLits (TypeError)

import HsBindgen.Runtime.CBool qualified as CBool
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.Support.FunPtr (ToFunPtr)

import HsBindgen.HighLevel (ToHighLevel, resultPure)
import HsBindgen.HighLevel.Internal.Errors (NoDefault, unreachable)
import HsBindgen.HighLevel.Marshaller
import HsBindgen.HighLevel.Marshaller.Utils (constByteStringLenIn, funPtrIn,
                                             nullConst, withCStringArrayIn,
                                             withCStringIn, withCStringMutIn,
                                             withConstIncompleteArrayIn)

{-------------------------------------------------------------------------------
  Default marshaller classes
-------------------------------------------------------------------------------}

-- | The default marshaller for a wrapper argument of type @hs@, filling the leading C
-- argument(s) of @lo@ and leaving @lo'@.
--
-- It is keyed on the Haskell type /and/ its C representation, so one Haskell type can
-- serve several C shapes. A 'ByteString' fills any @(const T *, len)@ pair, for
-- instance, with the pointer and length types taken from the C function rather than
-- fixed here.
--
-- The arrows in @lo@ are what say how many C arguments the marshaller consumes: one
-- for a single argument, two for a pair. Adding one of your own types takes a single
-- instance:
--
-- > newtype Handle = Handle CInt
-- > instance DefaultIn Handle (CInt -> lo) lo where
-- >   defaultIn = scalar (\(Handle h) -> h)
class DefaultIn hs lo lo' | hs lo -> lo' where
  defaultIn :: Marshal hs lo lo'

-- | The default marshaller for an out-parameter: how to read a filled C slot of type
-- @c@ back into a Haskell value of type @hs@.
--
-- The idiomatic scalars convert (a @'Ptr' 'Foreign.C.Types.CInt'@ reads back as an
-- 'Int', and so on), and a @'Ptr' a@ otherwise reads back as an @a@ unchanged, which
-- covers the raw C types and the newtypes a low-level binding wraps them in.
--
-- Buffers deliberately have no default. Reading a @char *@ or an array back needs a
-- length, and a length is not part of a type, so those stay explicit: see
-- @peekCStringOut@, @byteStringOut@ and @peekIncompleteArrayOut@ in
-- "HsBindgen.HighLevel.Marshaller.Utils".
--
-- Both parameters are already known at an 'HsBindgen.HighLevel.output' position, @c@
-- from the C function and @hs@ from the wrapper's result type, so neither needs to
-- determine the other.
--
class DefaultOut hs c where
  defaultOut :: Unmarshaller c hs

-- | The default conversion from a C return type @c@ to a Haskell result type @hs@.
--
-- As with 'DefaultIn', the type in the wrapper's signature is what picks: a @CInt@
-- return closes to @IO Int@ if the signature says 'Int' and stays @IO CInt@ if it says
-- @CInt@. Nothing here is inferred from the C side alone, so a wrapper written without
-- a result annotation is ambiguous.
--
-- This is a bare conversion rather than a whole closer because
-- "HsBindgen.HighLevel.Auto" has to splice it into a larger assembler when the spec
-- also has outputs. On its own, 'defaultRes' wraps it back up into a closer.
--
class DefaultRes c hs where
  defaultResConv :: c -> hs

{-------------------------------------------------------------------------------
  DefaultIn instances
-------------------------------------------------------------------------------}

-- Idiomatic Haskell scalars: convert to the canonical C type the binding uses.
instance DefaultIn Int    (CInt    -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Word   (CUInt   -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Double (CDouble -> lo) lo where defaultIn = scalar realToFrac
instance DefaultIn Float  (CFloat  -> lo) lo where defaultIn = scalar realToFrac
instance DefaultIn Bool   (CBool   -> lo) lo where defaultIn = scalar CBool.fromBool

-- Compound inputs: one Haskell value spread across its C representation. A
-- 'ByteString' fills any @(const T *, len)@ byte-buffer pair for an integral length,
-- so @auto@ resolves whichever pointer and length types the C function takes.
instance DefaultIn String (PtrConst CChar -> lo) lo where
  defaultIn = withCStringIn
instance DefaultIn String (Ptr CChar -> lo) lo where
  defaultIn = withCStringMutIn
instance DefaultIn [String] (PtrConst (PtrConst CChar) -> lo) lo where
  defaultIn = withCStringArrayIn
instance Integral len => DefaultIn ByteString (PtrConst a -> len -> lo) lo where
  defaultIn = constByteStringLenIn
instance Storable a => DefaultIn (IncompleteArray a) (PtrConst a -> lo) lo where
  defaultIn = withConstIncompleteArrayIn

-- Nullable const pointer: 'Nothing' fills NULL, 'Just' uses the underlying default,
-- so @auto@ fills a @Maybe String@ or a @Maybe@ over any pointer type.
instance DefaultIn hs (PtrConst a -> lo) lo
      => DefaultIn (Maybe hs) (PtrConst a -> lo) lo where
  defaultIn = marshalOptional ($ nullConst) defaultIn

-- Callback: a Haskell function passed as a C function pointer. The pointer is freed
-- when the call returns, so this fits a callback invoked during the call; one that C
-- retains past the call needs an explicit marshaller.
instance ToFunPtr a => DefaultIn a (FunPtr a -> lo) lo where
  defaultIn = funPtrIn

-- Identity: any type whose C argument is that same type passes through unchanged.
-- Covers every raw C scalar, fixed-width, and typedef, a raw or const pointer or
-- function pointer, and any generated type a wrapper keeps as-is.
instance DefaultIn a (a -> lo) lo where
  defaultIn = scalar id

{-------------------------------------------------------------------------------
  DefaultOut instances
-------------------------------------------------------------------------------}

-- Idiomatic Haskell scalars: coerce the peeked C scalar.
instance DefaultOut Int    (Ptr CInt)    where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Word   (Ptr CUInt)   where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Double (Ptr CDouble) where defaultOut = unmarshalOutPure realToFrac
instance DefaultOut Float  (Ptr CFloat)  where defaultOut = unmarshalOutPure realToFrac
instance DefaultOut Bool   (Ptr CBool)   where defaultOut = unmarshalOutPure CBool.toBool

-- Identity: peek any type whose out-parameter is a pointer to that same type. One
-- instance covers every raw C scalar, the fixed-width (stdint) types, the standard
-- library typedefs, a raw or const pointer or function pointer, and any generated
-- type a wrapper keeps unchanged.
instance Storable a => DefaultOut a (Ptr a) where
  defaultOut = unmarshalOutPure id

{-------------------------------------------------------------------------------
  DefaultRes instances
-------------------------------------------------------------------------------}

-- Idiomatic Haskell scalars: convert the C return value to its canonical Haskell type.
instance DefaultRes CInt    Int    where defaultResConv = fromIntegral
instance DefaultRes CUInt   Word   where defaultResConv = fromIntegral
instance DefaultRes CSize   Int    where defaultResConv = fromIntegral
instance DefaultRes CDouble Double where defaultResConv = realToFrac
instance DefaultRes CFloat  Float  where defaultResConv = realToFrac
instance DefaultRes CBool   Bool   where defaultResConv = CBool.toBool

-- Identity: any C return type comes back unchanged when the Haskell result type is
-- the same, covering @void@, every raw and fixed-width scalar, the typedefs, a raw
-- pointer or function pointer, and any generated type kept as-is.
instance DefaultRes c c where defaultResConv = id

-- | Close a spec that has no outputs by converting the C return value: drop it into
-- the closing position, as @... $ defaultRes@.
--
-- It is 'HsBindgen.HighLevel.Auto.autoResult' restricted to an output-free spec,
-- named here so the three defaults sit together. For a spec that has outputs, use
-- @autoResult@, which assembles them together with this conversion.
defaultRes :: DefaultRes c hs => ToHighLevel '[] (IO c) (IO hs)
defaultRes = resultPure defaultResConv
{-# INLINE defaultRes #-}

{-------------------------------------------------------------------------------
  "No default" fallbacks

  Overlappable instances that turn a missing default into a clear message. Each
  equates a parameter to the TypeError itself (at kind Type) so the message survives
  even when that parameter is concrete at the call site; do not "simplify" that away.
-------------------------------------------------------------------------------}

instance {-# OVERLAPPABLE #-} (lo' ~ TypeError (NoDefault "input" hs))
      => DefaultIn hs lo lo' where
  defaultIn = unreachable

instance {-# OVERLAPPABLE #-} (c ~ TypeError (NoDefault "output" hs))
      => DefaultOut hs c where
  defaultOut = unreachable

instance {-# OVERLAPPABLE #-} (hs ~ TypeError (NoDefault "result" c))
      => DefaultRes c hs where
  defaultResConv = unreachable
