{-# LANGUAGE MagicHash #-}
module HsBindgen.Runtime.FlexibleArrayMember (
    HasFlexibleArrayMember (..),
    HasFlexibleArrayLength (..),
    WithFlexibleArrayMember (..),
    peekWithFLAM,
) where

import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign
import GHC.Exts (Proxy#, proxy#)

class HasFlexibleArrayMember element struct | struct -> element where
  flexibleArrayMemberOffset :: Proxy# struct -> Int

class HasFlexibleArrayMember element struct => HasFlexibleArrayLength element struct | struct -> element where
  flexibleArrayMemberLength :: struct -> Int

data WithFlexibleArrayMember element struct = WithFlexibleArrayMember
    { flamStruct :: !struct
    , flamExtra  :: {-# UNPACK #-} !(VS.Vector element)
    }
  deriving Show

-- | Peek structure together with contents of flexible array member.
peekWithFLAM :: forall struct element. (Storable struct, Storable element, HasFlexibleArrayLength element struct)
    => Ptr struct -> IO (WithFlexibleArrayMember element struct)
peekWithFLAM ptr = do
    struct <- peek ptr
    let !len = flexibleArrayMemberLength struct
        !bytesN = len * sizeOf (undefined :: element)
    vector <- VSM.unsafeNew len
    withForeignPtr (fst (VSM.unsafeToForeignPtr0 vector)) $ \ptr' -> do
        copyBytes ptr' (plusPtr ptr (flexibleArrayMemberOffset (proxy# @struct))) bytesN
    vector' <- VS.unsafeFreeze vector
    return (WithFlexibleArrayMember struct vector')
