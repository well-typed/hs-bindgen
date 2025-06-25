{-# LANGUAGE ViewPatterns #-}
module HsBindgen.Runtime.ConstantArray (
    ConstantArray,
    repeat,
    toList,
    withPtr,
) where

import Prelude (IO, ($), Eq, Show, Int, undefined, fromIntegral, (*), return)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtr)
import Foreign.Storable (Storable (..))
import Foreign.Marshal.Utils (copyBytes)
import Data.Vector.Storable qualified as VS

import HsBindgen.Runtime.Marshal (ReadRaw, StaticSize, WriteRaw)

newtype ConstantArray (n :: Nat) a = CA (VS.Vector a)
  deriving (Eq, Show)
  deriving anyclass (ReadRaw, StaticSize, WriteRaw)

type role ConstantArray nominal nominal

repeat :: forall n a. (KnownNat n, Storable a) => a -> ConstantArray n a
repeat x = CA (VS.replicate (intVal (Proxy @n)) x)

toList :: Storable a => ConstantArray n a -> [a]
toList (CA v) = VS.toList v

-- | Retrieve the underlying pointer
--
-- Coercible abstraction to look through the `newtype`s wrappers of typedefs.
--
withPtr :: (Coercible b (ConstantArray n a), Storable a) => b -> (Ptr a -> IO a) -> IO a
withPtr (coerce -> CA v) k = do
    -- we copy the data, a e.g. int fun(int xs[3]) may mutate it.
    VS.MVector _ fptr <- VS.thaw v
    withForeignPtr fptr k

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
