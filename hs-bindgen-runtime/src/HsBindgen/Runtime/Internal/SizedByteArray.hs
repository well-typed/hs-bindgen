{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module HsBindgen.Runtime.Internal.SizedByteArray (
    SizedByteArray (..),
) where

import Data.Array.Byte (ByteArray (..))
import Data.Coerce (coerce)
import Data.Primitive (Prim (..))
import Data.Proxy (Proxy (..))
import Foreign (Storable (..))
import GHC.Base (Int (..))
import GHC.Exts qualified as Exts
import GHC.TypeNats qualified as GHC

import HsBindgen.Runtime.Internal.ByteArray
import HsBindgen.Runtime.Marshal

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A 'SizedByteArray.SizedByteArray' is a 'Data.Array.Byte.ByteArray' with
-- given size
--
-- 'SizedByteArray's provide deriving-via support for 'ByteArray'.
--
-- Intended usage:
--
-- > newtype Foo = Foo ByteArray
-- >   deriving (Storable, Prim) via SizedByteArray 16 4
--
-- == Size
--
-- In this example, the 'ByteArray' must have size 16.
--
-- == Storable
--
-- The derived 'Storable' instance does /not/ declare that the 'ByteArray'
-- /itself/ is memory aligned in any way (indeed, the 'ByteArray' may well not
-- be pinned). It merely states that if we use 'Storable' to pass the
-- 'ByteArray' to a C function, we must
--
-- * allocate a temporary buffer (typically using 'Foreign.alloca')
-- * copy the 'ByteArray' into that buffer
-- * call the C function, passing a pointer to this buffer
--
-- where /that temporary buffer/ must be memory aligned.
--
-- == Prim
--
-- Similar comments as for 'Storable' apply to 'Prim' also. Since 'Prim' can
-- be used to construct /arrays/, it's worth spelling out alignment requirements
-- in this case. If alignment is important, then typically /every/ element in
-- the array must be aligned. If the elements are stored in contiguous memory
-- locations, this will be the case only if
--
-- * the start of the array is memory aligned
--   (e.g., 'Data.Primitive.PrimArray.newAlignedPinnedPrimArray')
-- * the @size@ is an integral multiple of the @alignment@
--
-- If the @size@ and @alignment@ parameters originate from their values for a
-- choice of C type, then the second requirement will always be satisfied:
--
-- > The size of any type is always a multiple of its alignment; that way, in an
-- > array whose elements have that type, all the elements are properly aligned
-- > if the first one is.
--
-- <https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Type-Alignment.html>
newtype SizedByteArray (size :: GHC.Nat) (alignment :: GHC.Nat) =
    SizedByteArray ByteArray

{-------------------------------------------------------------------------------
  StaticSize, ReadRaw, WriteRaw
-------------------------------------------------------------------------------}

instance (GHC.KnownNat n, GHC.KnownNat m) => StaticSize (SizedByteArray n m) where
  staticSizeOf    _ = fromIntegral (GHC.natVal (Proxy @n))
  staticAlignment _ = fromIntegral (GHC.natVal (Proxy @m))

instance GHC.KnownNat n => ReadRaw (SizedByteArray n m) where
  readRaw = coerce $ peekByteArray (fromIntegral (GHC.natVal (Proxy @n)))

instance WriteRaw (SizedByteArray n m) where
  writeRaw = coerce $ pokeByteArray

{-------------------------------------------------------------------------------
  Storable
-------------------------------------------------------------------------------}

instance (GHC.KnownNat n, GHC.KnownNat m) => Storable (SizedByteArray n m) where
  sizeOf    _ = fromIntegral (GHC.natVal (Proxy @n))
  alignment _ = fromIntegral (GHC.natVal (Proxy @m))

  peek = coerce $ peekByteArray (fromIntegral (GHC.natVal (Proxy @n)))
  poke = coerce $ pokeByteArray

{-------------------------------------------------------------------------------
  Prim
-------------------------------------------------------------------------------}

instance (GHC.KnownNat n, GHC.KnownNat m) => Prim (SizedByteArray n m) where
  sizeOf# _ =
    case fromIntegral (GHC.natVal (Proxy @n)) of
      I# sz -> sz

  alignment# _ =
    case fromIntegral (GHC.natVal (Proxy @m)) of
      I# al -> al

  indexByteArray# src# i# =
    -- Create a new ByteArray# by copying a slice from the source
    -- This is safe to use unsafe operation because we're only reading
    case Exts.runRW# $ \s0# ->
      case Exts.newByteArray# size# s0# of
        (# s1#, dest# #) ->
          case Exts.copyByteArray# src# offset# dest# 0# size# s1# of
            s2# -> Exts.unsafeFreezeByteArray# dest# s2#
    of
      (# _, frozen# #) -> SizedByteArray (ByteArray frozen#)
    where
      size# = case fromIntegral (GHC.natVal (Proxy @n)) of I# s -> s
      offset# = i# Exts.*# size#

  readByteArray# src# i# s0# =
    case Exts.newByteArray# size# s0# of
      (# s1#, dest# #) ->
        case Exts.copyMutableByteArray# src# offset# dest# 0# size# s1# of
          s2# -> case Exts.unsafeFreezeByteArray# dest# s2# of
            (# s3#, frozen# #) -> (# s3#, SizedByteArray (ByteArray frozen#) #)
    where
      size# = case fromIntegral (GHC.natVal (Proxy @n)) of I# s -> s
      offset# = i# Exts.*# size#

  writeByteArray# dest# i# (SizedByteArray (ByteArray src#)) s# =
    Exts.copyByteArray# src# 0# dest# offset# size# s#
    where
      size# = case fromIntegral (GHC.natVal (Proxy @n)) of I# s -> s
      offset# = i# Exts.*# size#

  indexOffAddr# addr# i# =
    -- Similar to indexByteArray#, we need to copy from the address
    --
    case Exts.runRW# $ \s0# ->
      case Exts.newByteArray# size# s0# of
        (# s1#, dest# #) ->
          case Exts.copyAddrToByteArray# (Exts.plusAddr# addr# offset#) dest# 0# size# s1# of
            s2# -> Exts.unsafeFreezeByteArray# dest# s2#
    of
      (# _, frozen# #) -> SizedByteArray (ByteArray frozen#)
    where
      size# = case fromIntegral (GHC.natVal (Proxy @n)) of I# s -> s
      offset# = i# Exts.*# size#

  readOffAddr# addr# i# s0# =
    case Exts.newByteArray# size# s0# of
      (# s1#, dest# #) ->
        case Exts.copyAddrToByteArray# (Exts.plusAddr# addr# offset#) dest# 0# size# s1# of
          s2# -> case Exts.unsafeFreezeByteArray# dest# s2# of
            (# s3#, frozen# #) -> (# s3#, SizedByteArray (ByteArray frozen#) #)
    where
      size# = case fromIntegral (GHC.natVal (Proxy @n)) of I# s -> s
      offset# = i# Exts.*# size#

  writeOffAddr# addr# i# (SizedByteArray (ByteArray src#)) s# =
    Exts.copyByteArrayToAddr# src# 0# (Exts.plusAddr# addr# offset#) size# s#
    where
      size# = case fromIntegral (GHC.natVal (Proxy @n)) of I# s -> s
      offset# = i# Exts.*# size#
