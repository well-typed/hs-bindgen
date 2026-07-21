-- | The default marshaller for each Haskell type, so that the ordinary combinators in
-- a spec need no hand-written one.
--
-- One class per combinator: 'DefaultIn' for an @input@, 'DefaultOut' for an @output@,
-- 'DefaultRes' for the closer. Use them as @input defaultIn@,
-- @output defaultOut@ and @defaultRes@, or let "HsBindgen.HighLevel.Auto" fill them
-- in for you.
--
-- The Haskell type in the high-level signature picks the representation, the same way
-- in all three. The idiomatic scalars convert (@Int@ and @CInt@, @Bool@ and
-- @CBool@, @Double@ and @CDouble@, and so on) and everything else passes through
-- unchanged, so both of these compile and they mean different things:
--
-- > hsGetSize :: IncompleteArray Word8 -> IO Int    -- the int is converted to Int
-- > hsGetSize :: IncompleteArray Word8 -> IO CInt   -- the int is kept as it is
--
-- 'String', 'ByteString' and 'IncompleteArray' have input defaults too, and 'String'
-- and 'ByteString' have /result/ defaults: a C function that returns a
-- @const char *@ into memory it owns is copied out. Neither has an /output/ default,
-- because reading a caller-allocated buffer back needs a length, and a length is not
-- part of a type. (A returned @char *@ needs none: it is NUL-terminated.)
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
-- @unsafeByteStringLenIn@ at those combinators, which passes a pointer into the
-- 'ByteString' itself; it is @unsafe@ only in that C must not retain the pointer past
-- the call. Reaching for @auto@ picks the copying default, so a spec that needs the
-- other one has to say so.
--
-- __Warning: the string result defaults borrow.__ @'DefaultRes' ('PtrConst' 'CChar')
-- 'String'@ and its three siblings copy out of a pointer C returned and free nothing,
-- because the usual C function of that shape hands back memory it keeps owning. A
-- call that instead expects the caller to free needs an explicit closer that does so,
-- and a call whose pointer dies before the copy needs one that copies sooner. Neither
-- is visible in the type, so the choice is yours to make per function.
--
module HsBindgen.HighLevel.Defaults (
    DefaultIn (..)
  , DefaultOut (..)
  , DefaultRes (..)
  , defaultRes
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool, CChar, CDouble, CFloat, CInt, CLLong, CLong,
                        CPtrdiff, CSChar, CShort, CSize, CUChar, CUInt, CULLong,
                        CULong, CUShort)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (Storable)
import GHC.TypeLits (TypeError)

import HsBindgen.Runtime.CBool qualified as CBool
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import HsBindgen.Runtime.Support.FunPtr (ToFunPtr)

import HsBindgen.HighLevel (ToHighLevel, resultIO)
import HsBindgen.HighLevel.Internal.Errors (NoDefault, unreachable)
import HsBindgen.HighLevel.Marshaller
import HsBindgen.HighLevel.Marshaller.Utils (constByteStringLenIn, funPtrIn,
                                             nullConst, withCStringArrayIn,
                                             withCStringIn, withCStringMutIn,
                                             withConstIncompleteArrayIn)

{-------------------------------------------------------------------------------
  Default marshaller classes
-------------------------------------------------------------------------------}

-- | The default marshaller for a high-level argument of type @hs@, filling the leading
-- C argument(s) of @lo@ and leaving @lo'@.
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
-- Both parameters are already known at an 'HsBindgen.HighLevel.output', @c@ from the C
-- function and @hs@ from the high-level result type, so neither needs to determine the
-- other.
--
class DefaultOut hs c where
  defaultOut :: Unmarshaller c hs

-- | The default conversion from a C return type @c@ to a Haskell result type @hs@.
--
-- As with 'DefaultIn', the type in the high-level signature is what picks: a @CInt@
-- return closes to @IO Int@ if the signature says 'Int' and stays @IO CInt@ if it says
-- @CInt@. Nothing here is inferred from the C side alone, so a binding written without
-- a result annotation is ambiguous.
--
-- The conversion runs in 'IO', which is what lets a default do more than coerce a
-- scalar: the string defaults below copy out of memory C owns, and a default of your
-- own may free a pointer, consult errno, or throw. A pure conversion is @pure . f@:
--
-- > newtype Version = Version Int
-- > instance DefaultRes CInt Version where
-- >   defaultResConv = pure . Version . fromIntegral
--
-- This is a bare conversion rather than a whole closer because
-- "HsBindgen.HighLevel.Auto" has to splice it into a larger assembler when the spec
-- also has outputs. On its own, 'defaultRes' wraps it back up into a closer.
--
class DefaultRes c hs where
  defaultResConv :: c -> IO hs

{-------------------------------------------------------------------------------
  DefaultIn instances
-------------------------------------------------------------------------------}

-- Integral arguments: 'Int' fills any signed C integer argument and 'Word' any
-- unsigned one (except @size_t@, which maps to 'Int' as a length; see the 'DefaultRes'
-- instances below). The same rule the result default uses, so a C type converted as an
-- argument is converted the same way as a result.
instance DefaultIn Int  (CSChar   -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Int  (CShort   -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Int  (CInt     -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Int  (CLong    -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Int  (CLLong   -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Int  (CPtrdiff -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Int  (CSize    -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Word (CUChar   -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Word (CUShort  -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Word (CUInt    -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Word (CULong   -> lo) lo where defaultIn = scalar fromIntegral
instance DefaultIn Word (CULLong  -> lo) lo where defaultIn = scalar fromIntegral

-- The remaining idiomatic scalars.
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
-- function pointer, and any generated type a binding keeps as-is.
instance DefaultIn a (a -> lo) lo where
  defaultIn = scalar id

{-------------------------------------------------------------------------------
  DefaultOut instances
-------------------------------------------------------------------------------}

-- Integral out-parameters: the same widths as the argument and result defaults,
-- peeked and converted.
instance DefaultOut Int  (Ptr CSChar)   where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Int  (Ptr CShort)   where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Int  (Ptr CInt)     where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Int  (Ptr CLong)    where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Int  (Ptr CLLong)   where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Int  (Ptr CPtrdiff) where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Int  (Ptr CSize)    where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Word (Ptr CUChar)   where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Word (Ptr CUShort)  where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Word (Ptr CUInt)    where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Word (Ptr CULong)   where defaultOut = unmarshalOutPure fromIntegral
instance DefaultOut Word (Ptr CULLong)  where defaultOut = unmarshalOutPure fromIntegral

-- The remaining idiomatic scalars.
instance DefaultOut Double (Ptr CDouble) where defaultOut = unmarshalOutPure realToFrac
instance DefaultOut Float  (Ptr CFloat)  where defaultOut = unmarshalOutPure realToFrac
instance DefaultOut Bool   (Ptr CBool)   where defaultOut = unmarshalOutPure CBool.toBool

-- Identity: peek any type whose out-parameter is a pointer to that same type. The same
-- catch-all as the identity input default, covering every raw scalar, typedef, and
-- generated type a binding keeps unchanged.
instance Storable a => DefaultOut a (Ptr a) where
  defaultOut = unmarshalOutPure id

{-------------------------------------------------------------------------------
  DefaultRes instances
-------------------------------------------------------------------------------}

-- Integral returns: the same widths as the argument and out-parameter defaults.
-- @size_t@ maps to 'Int' rather than 'Word' in all three, because a @size_t@ is
-- nearly always a length and a Haskell length is an 'Int'.
instance DefaultRes CSChar   Int  where defaultResConv = pure . fromIntegral
instance DefaultRes CShort   Int  where defaultResConv = pure . fromIntegral
instance DefaultRes CInt     Int  where defaultResConv = pure . fromIntegral
instance DefaultRes CLong    Int  where defaultResConv = pure . fromIntegral
instance DefaultRes CLLong   Int  where defaultResConv = pure . fromIntegral
instance DefaultRes CPtrdiff Int  where defaultResConv = pure . fromIntegral
instance DefaultRes CSize    Int  where defaultResConv = pure . fromIntegral
instance DefaultRes CUChar   Word where defaultResConv = pure . fromIntegral
instance DefaultRes CUShort  Word where defaultResConv = pure . fromIntegral
instance DefaultRes CUInt    Word where defaultResConv = pure . fromIntegral
instance DefaultRes CULong   Word where defaultResConv = pure . fromIntegral
instance DefaultRes CULLong  Word where defaultResConv = pure . fromIntegral

-- The remaining idiomatic scalars.
instance DefaultRes CDouble Double where defaultResConv = pure . realToFrac
instance DefaultRes CFloat  Float  where defaultResConv = pure . realToFrac
instance DefaultRes CBool   Bool   where defaultResConv = pure . CBool.toBool

-- Borrowed strings: a C function returning @const char *@ into memory it owns (a
-- name, a path, a version) is copied out here, which needs 'IO'. It is a /copy of a
-- borrowed pointer/: C keeps ownership and nothing here frees anything, so a call
-- that hands back memory the caller must free needs an explicit closer that frees it.
--
-- These four dereference the pointer unconditionally. A C function that returns NULL
-- to mean "no value" needs the nullable form below, which is what the signature asks
-- for by saying @IO (Maybe String)@ rather than @IO String@.
instance DefaultRes (PtrConst CChar) String     where defaultResConv = peekCString . PtrConst.unsafeToPtr
instance DefaultRes (Ptr CChar)      String     where defaultResConv = peekCString
instance DefaultRes (PtrConst CChar) ByteString where defaultResConv = BS.packCString . PtrConst.unsafeToPtr
instance DefaultRes (Ptr CChar)      ByteString where defaultResConv = BS.packCString

-- Nullable returns: a NULL pointer becomes 'Nothing', anything else is the underlying
-- default. Over the identity this covers the @T *@ return that is NULL for \"not
-- found\" (@'Maybe' ('Ptr' T)@); over the string defaults, the nullable @const char *@
-- (@'Maybe' 'String'@).
instance DefaultRes (Ptr a) hs => DefaultRes (Ptr a) (Maybe hs) where
  defaultResConv = nullableRes id
instance DefaultRes (PtrConst a) hs => DefaultRes (PtrConst a) (Maybe hs) where
  defaultResConv = nullableRes PtrConst.unsafeToPtr

-- The rule both nullable instances follow, written once: view the returned pointer as
-- a 'Ptr' to test it, and hand the pointer /itself/ to the underlying default, which
-- is what makes this compose with the string conversions above rather than only with
-- the identity.
nullableRes :: DefaultRes c hs => (c -> Ptr a) -> c -> IO (Maybe hs)
nullableRes toPtr p
  | toPtr p == nullPtr = pure Nothing
  | otherwise          = Just <$> defaultResConv p
{-# INLINE nullableRes #-}

-- Identity: any C return type comes back unchanged when the Haskell result type is the
-- same. The identity catch-all again, and the one that also covers @void@.
instance DefaultRes c c where defaultResConv = pure

-- | Close a spec that has no outputs by converting the C return value: drop it in as
-- the closer, as @... $ defaultRes@.
--
-- It is 'HsBindgen.HighLevel.Auto.autoResult' restricted to an output-free spec,
-- named here so the three defaults sit together. For a spec that has outputs, use
-- @autoResult@, which assembles them together with this conversion.
defaultRes :: DefaultRes c hs => ToHighLevel '[] (IO c) (IO hs)
defaultRes = resultIO defaultResConv
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
