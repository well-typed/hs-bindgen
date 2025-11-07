{-# LANGUAGE MagicHash #-}
module HsBindgen.Runtime.FlexibleArrayMember (
    HasFlexibleArrayMember (..),
    HasFlexibleArrayLength (..),
    WithFlexibleArrayMember (..),
    peekWithFLAM,
    FLAMLengthMismatch (..),
    pokeWithFLAM,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
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
  deriving stock Show

-- | Peek structure with flexible array member.
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

data FLAMLengthMismatch = FLAMLengthMismatch { flamLengthStruct :: Int
                                             , flamLengthProvided :: Int }
  deriving stock (Show)

instance Exception FLAMLengthMismatch

-- | Poke structure with flexible array member.
pokeWithFLAM
  :: forall struct elem.
     (Storable struct, Storable elem, HasFlexibleArrayLength elem struct)
  => Ptr struct -> WithFlexibleArrayMember elem struct -> IO ()
pokeWithFLAM ptrToStruct (WithFlexibleArrayMember struct' vector')  = do
  struct <- peek ptrToStruct
  let !lenFLAM = flexibleArrayMemberLength struct
      !lenVector' = VS.length vector'
  unless (lenFLAM == lenVector') $ throwIO $ FLAMLengthMismatch lenFLAM lenVector'
  poke ptrToStruct struct'
  mVector' <- VS.unsafeThaw vector'
  withForeignPtr (fst (VSM.unsafeToForeignPtr0 mVector')) $ \ptrToVec -> do
    let !ptrToFLAM = plusPtr ptrToStruct (flexibleArrayMemberOffset (proxy# @struct))
        !bytesN = lenFLAM * sizeOf (undefined :: elem)
    copyBytes ptrToFLAM ptrToVec bytesN
  _ <- VS.unsafeFreeze mVector'
  pure ()
