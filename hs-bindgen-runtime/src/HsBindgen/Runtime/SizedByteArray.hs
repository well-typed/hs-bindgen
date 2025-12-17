{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module HsBindgen.Runtime.SizedByteArray (
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

import HsBindgen.Runtime.ByteArray

-- | The 'SizedByteArray'; we have two parameters, to specify the size and alignment.
newtype SizedByteArray (size :: GHC.Nat) (alignment :: GHC.Nat) = SizedByteArray ByteArray
  deriving newtype (Eq, Show) -- To avoid printing wrapper constructor

instance (GHC.KnownNat n, GHC.KnownNat m) => Storable (SizedByteArray n m) where
    sizeOf _ = fromIntegral (GHC.natVal (Proxy @n))
    alignment _ = fromIntegral (GHC.natVal (Proxy @m))

    peek = coerce $ peekByteArray (fromIntegral (GHC.natVal (Proxy @n)))
    poke = coerce pokeByteArray

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
