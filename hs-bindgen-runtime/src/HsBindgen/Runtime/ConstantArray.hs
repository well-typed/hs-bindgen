module HsBindgen.Runtime.ConstantArray (
    ConstantArray,
    repeat,
) where

import Prelude (($), Eq, Show, Int, undefined, fromIntegral, (*), return)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtr)
import Foreign.Storable (Storable (..))
import Foreign.Marshal.Utils (copyBytes)
import Data.Vector.Storable qualified as VS

newtype ConstantArray (n :: Nat) a = CA (VS.Vector a)
  deriving (Eq, Show)

type role ConstantArray nominal nominal

repeat :: forall n a. (KnownNat n, Storable a) => a -> ConstantArray n a
repeat x = CA (VS.replicate (intVal (Proxy @n)) x)

instance (Storable a, KnownNat n) => Storable (ConstantArray n a) where
    sizeOf _ = intVal (Proxy @n) * sizeOf (undefined :: a)

    alignment _ = alignment (undefined :: a)

    peek ptr = do
        fptr <- mallocForeignPtr
        withForeignPtr fptr $ \ptr' -> do
            copyBytes ptr' (castPtr ptr) (size * sizeOfA)
        vs <- VS.freeze (VS.MVector size fptr)
        return (CA vs)
      where
        size = intVal (Proxy @n)
        sizeOfA = sizeOf (undefined :: a)

    poke ptr (CA vs) = do
        VS.MVector size fptr <- VS.unsafeThaw vs
        withForeignPtr fptr $ \ptr' -> do
            copyBytes ptr (castPtr ptr') (size * sizeOfA)
      where
        sizeOfA = sizeOf (undefined :: a)

intVal :: forall n. KnownNat n => Proxy n -> Int
intVal p = fromIntegral (natVal p)
