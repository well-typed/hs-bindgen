module HsBindgen.Runtime.SizedByteArray (
    SizedByteArray (..),
) where

import Data.Array.Byte (ByteArray)
import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Foreign (Storable (..))
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
