-- 'AutoA' is ambiguous in isolation: @subLo@ resolves only once the low-level type
-- @lo@ is known at a ground call site.
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Default marshallers for 'ToHighLevel', so 'auto' and @input defaultIn@ \/
-- @output defaultOut@ \/ @defaultRes@ fill mundane positions with no hand-written
-- marshaller. In every position the Haskell type you write in the wrapper signature
-- picks the representation, the same way for an argument, an output, and the result:
--
--   * The idiomatic Haskell scalars convert to and from their canonical C type
--     (@Int@ ↔ @CInt@, @Word@ ↔ @CUInt@, @Bool@ ↔ @CBool@, @CSize@ -> @Int@, and so
--     on), so a wrapper written in idiomatic Haskell needs no marshaller.
--   * A single general identity covers /any/ type whose C representation is that same
--     type, on both the input side (an argument passed through) and the result (a C
--     return kept as-is): every raw C scalar, fixed-width, and typedef, a raw or const
--     pointer or function pointer, and any generated type a wrapper keeps unchanged.
--     Only the output side keeps a per-type identity (the C integer, character, and
--     floating types, @CSize@ \/ @CPtrdiff@, the wide-character, time, @intmax@,
--     @intptr@, @sig_atomic@, and Unicode typedefs, and the fixed-width types), since
--     an output is keyed on its Haskell type alone.
--   * @const char *@ ↔ 'String', a 'Data.ByteString.ByteString' as any
--     @(const T *, len)@ byte-buffer pair for an integral length (@auto@ takes the
--     pointer and length types from the C function), and @const T *@ ↔
--     'IncompleteArray' cover the compound inputs; a buffer output or result has no
--     canonical length, so it stays explicit.
--
-- The result position works like the others: a C return type has both an idiomatic
-- and an identity default (@IO Int@ converts an @int@, @IO CInt@ keeps it), so write
-- the result type in the signature. 'defaultRes' reads the pair from the signature and
-- does not infer it, so an unannotated result is ambiguous.
--
-- This module is policy; for a different representation use an explicit marshaller and
-- do not import it.
--
-- __Warning: silent numeric conversion.__ The idiomatic scalar defaults coerce with
-- 'fromIntegral' \/ 'realToFrac', which are lossy and silent: @Int -> CInt@ truncates a
-- 64-bit 'Int' to 32 bits, and @CSize -> Int@ can wrap a large @size_t@ negative. Where
-- a value can fall outside the target range, keep the generated C type (its identity
-- default) or use an explicit checked marshaller.
--
module HsBindgen.HighLevel.Defaults (
    -- * Default marshallers
    --
    -- | Use as @input defaultIn@, @output defaultOut@, and @defaultRes@.
    DefaultIn (..)
  , DefaultOut (..)
  , DefaultRes (..)
    -- * Filling defaults automatically
    --
    -- | 'auto' fills the mundane input positions and the closer from the high-level
    -- signature; outputs stay explicit.
  , auto
  , Auto
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CBool, CChar, CClock, CDouble, CFloat, CInt, CIntMax,
                        CIntPtr, CLLong, CLong, CPtrdiff, CSChar, CShort,
                        CSigAtomic, CSize, CTime, CUChar, CUInt, CUIntMax,
                        CUIntPtr, CULLong, CULong, CUShort, CWchar)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable)
import GHC.TypeLits (TypeError)

import HsBindgen.Runtime.CBool qualified as CBool
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.LibC (CChar16T, CChar32T, CWctransT, CWctypeT, CWintT)
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.Support.FunPtr (ToFunPtr)

import HsBindgen.HighLevel (ToHighLevel, input, resultPure)
import HsBindgen.HighLevel.Internal.Errors (AutoMismatch, NoDefault)
import HsBindgen.HighLevel.Internal.Threading (ThreadIn)
import HsBindgen.HighLevel.Marshaller
import HsBindgen.HighLevel.Marshaller.Utils (constByteStringLenIn, funPtrIn,
                                             nullConst, withCStringArrayIn,
                                             withCStringIn, withCStringMutIn,
                                             withConstIncompleteArrayIn)

{-------------------------------------------------------------------------------
  Default marshaller classes
-------------------------------------------------------------------------------}

-- | The default input marshaller for a Haskell type @hs@, filling the leading C
-- argument(s) @lo@ down to the remainder @subLo@. Keyed on both the Haskell type and
-- its C representation, so one Haskell type may serve several: a 'ByteString' fills
-- any @(const T *, len)@ byte-buffer pair, and @auto@ picks whichever the C function
-- takes. A binding adds its own type in one instance:
--
-- > instance DefaultIn Handle (CInt -> lo) lo where   -- newtype Handle = Handle CInt
-- >   defaultIn = scalar (\(Handle h) -> h)
class DefaultIn hs lo subLo | hs lo -> subLo where
  defaultIn :: Marshal hs lo subLo

-- | The canonical output marshaller for a Haskell type @hs@, coercing a C scalar
-- to a Haskell scalar. Buffer-sized outputs (strings, arrays) have no canonical
-- length, so they have no default and stay explicit.
--
class DefaultOut hs c | hs -> c where
  defaultOut :: Unmarshaller c hs

-- | The result closer for a C return type @c@ exposed as the Haskell type @hs@: drop
-- it straight into a spec in the closing position, as @... $ defaultRes@. Like
-- 'DefaultIn', the @hs@ in the wrapper signature picks the representation, so a @CInt@
-- return closes to 'Int' (@IO Int@) or stays raw (@IO CInt@). There is no @c -> hs@
-- dependency: the pair is read from the signature, so write the result type rather
-- than leaving it inferred.
--
class DefaultRes c hs where
  defaultRes :: ToHighLevel (IO c) (IO hs)

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

-- Identity: peek a raw C scalar unchanged.
instance DefaultOut CChar    (Ptr CChar)    where defaultOut = unmarshalOutPure id
instance DefaultOut CSChar   (Ptr CSChar)   where defaultOut = unmarshalOutPure id
instance DefaultOut CUChar   (Ptr CUChar)   where defaultOut = unmarshalOutPure id
instance DefaultOut CShort   (Ptr CShort)   where defaultOut = unmarshalOutPure id
instance DefaultOut CUShort  (Ptr CUShort)  where defaultOut = unmarshalOutPure id
instance DefaultOut CInt     (Ptr CInt)     where defaultOut = unmarshalOutPure id
instance DefaultOut CUInt    (Ptr CUInt)    where defaultOut = unmarshalOutPure id
instance DefaultOut CLong    (Ptr CLong)    where defaultOut = unmarshalOutPure id
instance DefaultOut CULong   (Ptr CULong)   where defaultOut = unmarshalOutPure id
instance DefaultOut CLLong   (Ptr CLLong)   where defaultOut = unmarshalOutPure id
instance DefaultOut CULLong  (Ptr CULLong)  where defaultOut = unmarshalOutPure id
instance DefaultOut CSize    (Ptr CSize)    where defaultOut = unmarshalOutPure id
instance DefaultOut CPtrdiff (Ptr CPtrdiff) where defaultOut = unmarshalOutPure id
instance DefaultOut CBool    (Ptr CBool)    where defaultOut = unmarshalOutPure id
instance DefaultOut CFloat   (Ptr CFloat)   where defaultOut = unmarshalOutPure id
instance DefaultOut CDouble  (Ptr CDouble)  where defaultOut = unmarshalOutPure id

-- Identity: peek a fixed-width (stdint) type unchanged.
instance DefaultOut Int8   (Ptr Int8)   where defaultOut = unmarshalOutPure id
instance DefaultOut Int16  (Ptr Int16)  where defaultOut = unmarshalOutPure id
instance DefaultOut Int32  (Ptr Int32)  where defaultOut = unmarshalOutPure id
instance DefaultOut Int64  (Ptr Int64)  where defaultOut = unmarshalOutPure id
instance DefaultOut Word8  (Ptr Word8)  where defaultOut = unmarshalOutPure id
instance DefaultOut Word16 (Ptr Word16) where defaultOut = unmarshalOutPure id
instance DefaultOut Word32 (Ptr Word32) where defaultOut = unmarshalOutPure id
instance DefaultOut Word64 (Ptr Word64) where defaultOut = unmarshalOutPure id

-- Identity: peek a standard-library typedef scalar unchanged.
instance DefaultOut CWchar     (Ptr CWchar)     where defaultOut = unmarshalOutPure id
instance DefaultOut CTime      (Ptr CTime)      where defaultOut = unmarshalOutPure id
instance DefaultOut CClock     (Ptr CClock)     where defaultOut = unmarshalOutPure id
instance DefaultOut CIntMax    (Ptr CIntMax)    where defaultOut = unmarshalOutPure id
instance DefaultOut CUIntMax   (Ptr CUIntMax)   where defaultOut = unmarshalOutPure id
instance DefaultOut CIntPtr    (Ptr CIntPtr)    where defaultOut = unmarshalOutPure id
instance DefaultOut CUIntPtr   (Ptr CUIntPtr)   where defaultOut = unmarshalOutPure id
instance DefaultOut CSigAtomic (Ptr CSigAtomic) where defaultOut = unmarshalOutPure id
instance DefaultOut CWintT     (Ptr CWintT)     where defaultOut = unmarshalOutPure id
instance DefaultOut CChar16T   (Ptr CChar16T)   where defaultOut = unmarshalOutPure id
instance DefaultOut CChar32T   (Ptr CChar32T)   where defaultOut = unmarshalOutPure id
instance DefaultOut CWctransT  (Ptr CWctransT)  where defaultOut = unmarshalOutPure id
instance DefaultOut CWctypeT   (Ptr CWctypeT)   where defaultOut = unmarshalOutPure id

-- Identity: peek a Basic FFI character or a raw pointer unchanged.
instance DefaultOut Char       (Ptr Char)       where defaultOut = unmarshalOutPure id
instance DefaultOut (Ptr a)    (Ptr (Ptr a))    where defaultOut = unmarshalOutPure id
instance DefaultOut (FunPtr a) (Ptr (FunPtr a)) where defaultOut = unmarshalOutPure id

{-------------------------------------------------------------------------------
  DefaultRes instances
-------------------------------------------------------------------------------}

-- Idiomatic Haskell scalars: convert the C return value to its canonical Haskell type.
instance DefaultRes CInt    Int    where defaultRes = resultPure fromIntegral
instance DefaultRes CUInt   Word   where defaultRes = resultPure fromIntegral
instance DefaultRes CSize   Int    where defaultRes = resultPure fromIntegral
instance DefaultRes CDouble Double where defaultRes = resultPure realToFrac
instance DefaultRes CFloat  Float  where defaultRes = resultPure realToFrac
instance DefaultRes CBool   Bool   where defaultRes = resultPure CBool.toBool

-- Identity: any C return type comes back unchanged when the Haskell result type is
-- the same, covering @void@, every raw and fixed-width scalar, the typedefs, a raw
-- pointer or function pointer, and any generated type kept as-is.
instance DefaultRes c c where defaultRes = resultPure id

{-------------------------------------------------------------------------------
  "No default" fallbacks

  Overlappable instances that turn a missing default into a clear message. Each
  equates a parameter to the 'TypeError' itself (kind 'Type') so the message survives
  even when that parameter is concrete at the call site; do not "simplify" that away.
-------------------------------------------------------------------------------}

instance {-# OVERLAPPABLE #-} (subLo ~ TypeError (NoDefault "input" hs))
      => DefaultIn hs lo subLo where
  defaultIn = errorWithoutStackTrace "HighLevel.Defaults: unreachable"

instance {-# OVERLAPPABLE #-} (c ~ TypeError (NoDefault "output" hs))
      => DefaultOut hs c where
  defaultOut = errorWithoutStackTrace "HighLevel.Defaults: unreachable"

instance {-# OVERLAPPABLE #-} (hs ~ TypeError (NoDefault "result" c))
      => DefaultRes c hs where
  defaultRes = errorWithoutStackTrace "HighLevel.Defaults: unreachable"

{-------------------------------------------------------------------------------
  auto: fill the mundane positions from the signature
-------------------------------------------------------------------------------}

-- | The recursion behind 'auto': walk the high-level type @hi@ and the callable @lo@
-- in step, each @a ->@ in @hi@ becoming one @input defaultIn@ and the final @IO _@
-- the closer. Users reach for 'auto', not 'AutoA'.
class AutoA hi lo where
  autoA :: ToHighLevel lo hi

instance DefaultRes c hs => AutoA (IO hs) (IO c) where
  autoA = defaultRes
  {-# INLINE autoA #-}

instance ( DefaultIn a lo subLo
         , AutoA rest subLo
         , ThreadIn subLo rest
         )
      => AutoA (a -> rest) lo where
  autoA = input (defaultIn @a @lo @subLo) (autoA @rest @subLo)
  {-# INLINE autoA #-}

-- Fallback: neither real instance matched, so @hi@ bottomed out at a result while
-- @lo@ still takes arguments. Turns "No instance for AutoA ..." into a domain message.
instance {-# OVERLAPPABLE #-} TypeError (AutoMismatch hi lo) => AutoA hi lo where
  autoA = errorWithoutStackTrace "HighLevel.Defaults: unreachable"

-- | Fill a wrapper's mundane positions with their defaults, read from the
-- high-level signature (so write the signature first). Two uses, by how it is
-- applied:
--
--   * As a value, @... $ auto@, it fills every remaining input and closes with
--     'defaultRes'; so @output o $ auto@ keeps the explicit output and @auto@ does
--     the rest.
--   * Applied, @auto $ rest@, it fills the next input and continues with @rest@.
--
-- It fills inputs and the closer only; outputs stay explicit. The closer is
-- 'defaultRes', which reads the result type from the signature, so always give the
-- wrapper a type; an unannotated result is ambiguous. A position with no
-- default ('NoDefault') or a C argument it cannot place ('AutoMismatch', e.g. an
-- out-parameter) is a type error naming the fix.
class Auto r where
  auto :: r

instance AutoA hi lo => Auto (ToHighLevel lo hi) where
  auto = autoA
  {-# INLINE auto #-}

instance ( DefaultIn a lo sub
         , ThreadIn sub hcont
         , hi ~ (a -> hcont)
         )
      => Auto (ToHighLevel sub hcont -> ToHighLevel lo hi) where
  auto = input (defaultIn @a @lo @sub)
  {-# INLINE auto #-}
