-- The "no default" fallback instance below omits its 'DefInArrow' equation (its
-- 'TypeError' fires first), which trips -Wmissing-methods; silenced here.
{-# OPTIONS_GHC -Wno-missing-methods #-}
-- 'AutoA' is ambiguous in isolation: @subLo@ resolves only once 'DefInArrow'
-- reduces at a ground call site.
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Default marshallers for 'ToHighLevel', so 'auto' and @input defaultIn@ \/
-- @output defaultOut@ \/ @defaultRes@ fill mundane positions with no hand-written
-- marshaller. In every position the Haskell type you write in the wrapper signature
-- picks the representation, the same way for an argument, an output, and the result:
--
--   * The idiomatic Haskell scalars convert to and from their canonical C type
--     (@Int@ ↔ @CInt@, @Word@ ↔ @CUInt@, @Bool@ ↔ @CBool@, @CSize@ -> @Int@, and so
--     on), so a wrapper written in idiomatic Haskell needs no marshaller.
--   * Every raw C scalar type also has an identity input, output, and result default:
--     the C integer, character, and floating types (@CChar@ through @CDouble@),
--     @CSize@ and @CPtrdiff@, the wide-character, time, @intmax@, @intptr@, and
--     @sig_atomic@ typedefs (@CWchar@, @CTime@, @CClock@, @CIntMax@, @CUIntMax@,
--     @CIntPtr@, @CUIntPtr@, @CSigAtomic@), the C11 and C23 Unicode typedefs
--     (@CWintT@, @CChar16T@, @CChar32T@, @CWctransT@, @CWctypeT@), and the fixed-width
--     types (@Int8@..@Int64@, @Word8@..@Word64@). A raw pointer or function pointer
--     (@Ptr@, @FunPtr@) and the Basic FFI @Char@ pass through the same way, so a
--     wrapper that keeps the generated type passes the value through unchanged.
--   * @const char *@ ↔ 'String', the @(const char *, size_t)@ pair ↔
--     'Data.ByteString.ByteString', and @const T *@ ↔ 'IncompleteArray' cover the
--     compound inputs; a buffer output or result has no canonical length, so it stays
--     explicit.
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
module HsBindgen.Runtime.HighLevel.Defaults (
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
import Data.Kind (Type)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CBool, CChar, CClock, CDouble, CFloat, CInt, CIntMax,
                        CIntPtr, CLLong, CLong, CPtrdiff, CSChar, CShort,
                        CSigAtomic, CSize, CTime, CUChar, CUInt, CUIntMax,
                        CUIntPtr, CULLong, CULong, CUShort, CWchar)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable)
import GHC.TypeLits (TypeError)

import HsBindgen.Runtime.CBool qualified as CBool
import HsBindgen.Runtime.HighLevel (ToHighLevel, input, resultPure)
import HsBindgen.Runtime.HighLevel.Internal.Errors (AutoMismatch, NoDefault)
import HsBindgen.Runtime.HighLevel.Internal.Threading (ThreadIn)
import HsBindgen.Runtime.HighLevel.Marshaller
import HsBindgen.Runtime.HighLevel.Marshaller.Utils (useAsByteStringLenIn,
                                                     withCStringIn,
                                                     withConstIncompleteArrayIn)
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.LibC (CChar16T, CChar32T, CWctransT, CWctypeT, CWintT)
import HsBindgen.Runtime.PtrConst (PtrConst)

{-------------------------------------------------------------------------------
  Default marshaller classes
-------------------------------------------------------------------------------}

-- | The canonical input marshaller for a Haskell type @hs@. The associated family
-- 'DefInArrow' fixes the C argument(s) it fills (a scalar one, a 'ByteString' the
-- @(const char *, size_t)@ pair), which lets @input defaultIn@ infer the C side
-- with no annotation. A binding adds its own type in one instance block:
--
-- > instance DefaultIn Handle where        -- newtype Handle = Handle CInt
-- >   type DefInArrow Handle lo = CInt -> lo
-- >   defaultIn = scalar (\(Handle h) -> h)
class DefaultIn hs where
  type DefInArrow hs lo :: Type
  defaultIn :: Marshal hs (DefInArrow hs lo) lo

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
instance DefaultIn Int    where { type DefInArrow Int    lo = CInt    -> lo; defaultIn = scalar fromIntegral   }
instance DefaultIn Word   where { type DefInArrow Word   lo = CUInt   -> lo; defaultIn = scalar fromIntegral   }
instance DefaultIn Double where { type DefInArrow Double lo = CDouble -> lo; defaultIn = scalar realToFrac     }
instance DefaultIn Float  where { type DefInArrow Float  lo = CFloat  -> lo; defaultIn = scalar realToFrac     }
instance DefaultIn Bool   where { type DefInArrow Bool   lo = CBool   -> lo; defaultIn = scalar CBool.fromBool }

-- Compound inputs: one Haskell value spread across its C representation.
instance DefaultIn String where
  type DefInArrow String lo = PtrConst CChar -> lo
  defaultIn = withCStringIn
instance DefaultIn ByteString where
  type DefInArrow ByteString lo = PtrConst CChar -> CSize -> lo
  defaultIn = useAsByteStringLenIn
instance Storable a => DefaultIn (IncompleteArray a) where
  type DefInArrow (IncompleteArray a) lo = PtrConst a -> lo
  defaultIn = withConstIncompleteArrayIn

-- Identity: keep a raw C scalar exactly as the binding produced it.
instance DefaultIn CChar    where { type DefInArrow CChar    lo = CChar    -> lo; defaultIn = scalar id }
instance DefaultIn CSChar   where { type DefInArrow CSChar   lo = CSChar   -> lo; defaultIn = scalar id }
instance DefaultIn CUChar   where { type DefInArrow CUChar   lo = CUChar   -> lo; defaultIn = scalar id }
instance DefaultIn CShort   where { type DefInArrow CShort   lo = CShort   -> lo; defaultIn = scalar id }
instance DefaultIn CUShort  where { type DefInArrow CUShort  lo = CUShort  -> lo; defaultIn = scalar id }
instance DefaultIn CInt     where { type DefInArrow CInt     lo = CInt     -> lo; defaultIn = scalar id }
instance DefaultIn CUInt    where { type DefInArrow CUInt    lo = CUInt    -> lo; defaultIn = scalar id }
instance DefaultIn CLong    where { type DefInArrow CLong    lo = CLong    -> lo; defaultIn = scalar id }
instance DefaultIn CULong   where { type DefInArrow CULong   lo = CULong   -> lo; defaultIn = scalar id }
instance DefaultIn CLLong   where { type DefInArrow CLLong   lo = CLLong   -> lo; defaultIn = scalar id }
instance DefaultIn CULLong  where { type DefInArrow CULLong  lo = CULLong  -> lo; defaultIn = scalar id }
instance DefaultIn CSize    where { type DefInArrow CSize    lo = CSize    -> lo; defaultIn = scalar id }
instance DefaultIn CPtrdiff where { type DefInArrow CPtrdiff lo = CPtrdiff -> lo; defaultIn = scalar id }
instance DefaultIn CBool    where { type DefInArrow CBool    lo = CBool    -> lo; defaultIn = scalar id }
instance DefaultIn CFloat   where { type DefInArrow CFloat   lo = CFloat   -> lo; defaultIn = scalar id }
instance DefaultIn CDouble  where { type DefInArrow CDouble  lo = CDouble  -> lo; defaultIn = scalar id }

-- Identity: keep a fixed-width (stdint) type exactly.
instance DefaultIn Int8   where { type DefInArrow Int8   lo = Int8   -> lo; defaultIn = scalar id }
instance DefaultIn Int16  where { type DefInArrow Int16  lo = Int16  -> lo; defaultIn = scalar id }
instance DefaultIn Int32  where { type DefInArrow Int32  lo = Int32  -> lo; defaultIn = scalar id }
instance DefaultIn Int64  where { type DefInArrow Int64  lo = Int64  -> lo; defaultIn = scalar id }
instance DefaultIn Word8  where { type DefInArrow Word8  lo = Word8  -> lo; defaultIn = scalar id }
instance DefaultIn Word16 where { type DefInArrow Word16 lo = Word16 -> lo; defaultIn = scalar id }
instance DefaultIn Word32 where { type DefInArrow Word32 lo = Word32 -> lo; defaultIn = scalar id }
instance DefaultIn Word64 where { type DefInArrow Word64 lo = Word64 -> lo; defaultIn = scalar id }

-- Identity: keep a standard-library typedef scalar exactly.
instance DefaultIn CWchar     where { type DefInArrow CWchar     lo = CWchar     -> lo; defaultIn = scalar id }
instance DefaultIn CTime      where { type DefInArrow CTime      lo = CTime      -> lo; defaultIn = scalar id }
instance DefaultIn CClock     where { type DefInArrow CClock     lo = CClock     -> lo; defaultIn = scalar id }
instance DefaultIn CIntMax    where { type DefInArrow CIntMax    lo = CIntMax    -> lo; defaultIn = scalar id }
instance DefaultIn CUIntMax   where { type DefInArrow CUIntMax   lo = CUIntMax   -> lo; defaultIn = scalar id }
instance DefaultIn CIntPtr    where { type DefInArrow CIntPtr    lo = CIntPtr    -> lo; defaultIn = scalar id }
instance DefaultIn CUIntPtr   where { type DefInArrow CUIntPtr   lo = CUIntPtr   -> lo; defaultIn = scalar id }
instance DefaultIn CSigAtomic where { type DefInArrow CSigAtomic lo = CSigAtomic -> lo; defaultIn = scalar id }
instance DefaultIn CWintT     where { type DefInArrow CWintT     lo = CWintT     -> lo; defaultIn = scalar id }
instance DefaultIn CChar16T   where { type DefInArrow CChar16T   lo = CChar16T   -> lo; defaultIn = scalar id }
instance DefaultIn CChar32T   where { type DefInArrow CChar32T   lo = CChar32T   -> lo; defaultIn = scalar id }
instance DefaultIn CWctransT  where { type DefInArrow CWctransT  lo = CWctransT  -> lo; defaultIn = scalar id }
instance DefaultIn CWctypeT   where { type DefInArrow CWctypeT   lo = CWctypeT   -> lo; defaultIn = scalar id }

-- Identity: a Basic FFI character, raw pointer, or function pointer passes through.
instance DefaultIn Char       where { type DefInArrow Char       lo = Char       -> lo; defaultIn = scalar id }
instance DefaultIn (Ptr a)    where { type DefInArrow (Ptr a)    lo = Ptr a      -> lo; defaultIn = scalar id }
instance DefaultIn (FunPtr a) where { type DefInArrow (FunPtr a) lo = FunPtr a   -> lo; defaultIn = scalar id }

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

-- Identity: return the raw C scalar (or void) unchanged.
instance DefaultRes ()       ()       where defaultRes = resultPure id
instance DefaultRes CChar    CChar    where defaultRes = resultPure id
instance DefaultRes CSChar   CSChar   where defaultRes = resultPure id
instance DefaultRes CUChar   CUChar   where defaultRes = resultPure id
instance DefaultRes CShort   CShort   where defaultRes = resultPure id
instance DefaultRes CUShort  CUShort  where defaultRes = resultPure id
instance DefaultRes CInt     CInt     where defaultRes = resultPure id
instance DefaultRes CUInt    CUInt    where defaultRes = resultPure id
instance DefaultRes CLong    CLong    where defaultRes = resultPure id
instance DefaultRes CULong   CULong   where defaultRes = resultPure id
instance DefaultRes CLLong   CLLong   where defaultRes = resultPure id
instance DefaultRes CULLong  CULLong  where defaultRes = resultPure id
instance DefaultRes CSize    CSize    where defaultRes = resultPure id
instance DefaultRes CPtrdiff CPtrdiff where defaultRes = resultPure id
instance DefaultRes CBool    CBool    where defaultRes = resultPure id
instance DefaultRes CFloat   CFloat   where defaultRes = resultPure id
instance DefaultRes CDouble  CDouble  where defaultRes = resultPure id

-- Identity: return a standard-library typedef scalar unchanged.
instance DefaultRes CWchar     CWchar     where defaultRes = resultPure id
instance DefaultRes CTime      CTime      where defaultRes = resultPure id
instance DefaultRes CClock     CClock     where defaultRes = resultPure id
instance DefaultRes CIntMax    CIntMax    where defaultRes = resultPure id
instance DefaultRes CUIntMax   CUIntMax   where defaultRes = resultPure id
instance DefaultRes CIntPtr    CIntPtr    where defaultRes = resultPure id
instance DefaultRes CUIntPtr   CUIntPtr   where defaultRes = resultPure id
instance DefaultRes CSigAtomic CSigAtomic where defaultRes = resultPure id
instance DefaultRes CWintT     CWintT     where defaultRes = resultPure id
instance DefaultRes CChar16T   CChar16T   where defaultRes = resultPure id
instance DefaultRes CChar32T   CChar32T   where defaultRes = resultPure id
instance DefaultRes CWctransT  CWctransT  where defaultRes = resultPure id
instance DefaultRes CWctypeT   CWctypeT   where defaultRes = resultPure id

-- Identity: return a fixed-width (stdint) type unchanged.
instance DefaultRes Int8   Int8   where defaultRes = resultPure id
instance DefaultRes Int16  Int16  where defaultRes = resultPure id
instance DefaultRes Int32  Int32  where defaultRes = resultPure id
instance DefaultRes Int64  Int64  where defaultRes = resultPure id
instance DefaultRes Word8  Word8  where defaultRes = resultPure id
instance DefaultRes Word16 Word16 where defaultRes = resultPure id
instance DefaultRes Word32 Word32 where defaultRes = resultPure id
instance DefaultRes Word64 Word64 where defaultRes = resultPure id

-- Identity: return a Basic FFI character, raw pointer, or function pointer unchanged.
instance DefaultRes Char       Char       where defaultRes = resultPure id
instance DefaultRes (Ptr a)    (Ptr a)    where defaultRes = resultPure id
instance DefaultRes (FunPtr a) (FunPtr a) where defaultRes = resultPure id

{-------------------------------------------------------------------------------
  "No default" fallbacks

  Overlappable instances that turn a missing default into a clear message. The output
  and result fallbacks equate their second parameter to the 'TypeError' itself (kind
  'Type') so the message survives even when that parameter is concrete at the call
  site; do not "simplify" that away.
-------------------------------------------------------------------------------}

-- Omits the 'DefInArrow' equation on purpose (see the -Wno-missing-methods note
-- at the top of the module): the 'TypeError' fires before the family is read.
instance {-# OVERLAPPABLE #-} TypeError (NoDefault "input" hs) => DefaultIn hs where
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

-- | The recursion behind 'auto': walk the high-level type @hi@ and the callable
-- @lo@ in step. Read it as a rule: each @a ->@ in @hi@ becomes one
-- @input defaultIn@, and the final @IO _@ becomes the closer ('defaultRes').
-- Driven by @hi@, so the non-injective 'DefInArrow' is fine. Users reach for
-- 'auto', not 'AutoA'.
class AutoA hi lo where
  autoA :: ToHighLevel lo hi

instance DefaultRes c hs => AutoA (IO hs) (IO c) where
  autoA = defaultRes
  {-# INLINE autoA #-}

instance ( DefaultIn a
         , AutoA rest subLo
         , ThreadIn subLo rest
         , lo ~ DefInArrow a subLo
         )
      => AutoA (a -> rest) lo where
  autoA = input (defaultIn @a) (autoA @rest @subLo)
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

instance ( DefaultIn a
         , ThreadIn sub hcont
         , lo ~ DefInArrow a sub
         , hi ~ (a -> hcont)
         )
      => Auto (ToHighLevel sub hcont -> ToHighLevel lo hi) where
  auto = input (defaultIn @a)
  {-# INLINE auto #-}
