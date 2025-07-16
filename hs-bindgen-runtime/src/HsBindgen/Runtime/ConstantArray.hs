{-# LANGUAGE ViewPatterns #-}
module HsBindgen.Runtime.ConstantArray (
    ConstantArray
    -- * Construction
  , repeat
  , fromList
    -- * Query
  , toVector
  , toList
  , withPtr
  ) where

import Prelude hiding (repeat)

import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy (..))
import Data.Vector.Storable qualified as VS
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.TypeNats (KnownNat, Nat, natVal)

import HsBindgen.Runtime.Marshal (ReadRaw, StaticSize, WriteRaw)
import GHC.Stack

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype ConstantArray (n :: Nat) a = CA (VS.Vector a)
  deriving (Eq, Show)
  deriving anyclass (ReadRaw, StaticSize, WriteRaw)

type role ConstantArray nominal nominal

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

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

repeat :: forall n a. (KnownNat n, Storable a) => a -> ConstantArray n a
repeat x = CA (VS.replicate (intVal (Proxy @n)) x)

-- | Construct 'ConstantArray' from list
--
-- Precondition: the list must have the right number of elements.
fromList :: forall n a.
     (KnownNat n, Storable a, HasCallStack)
  => [a] -> ConstantArray n a
fromList xs
  | length xs == n = CA (VS.fromList xs)
  | otherwise = error $ "fromList: expected " ++ show n ++ " elements"
  where
    n = intVal (Proxy @n)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Get the underlying 'Vector' representation (@O(1)@)
--
-- This loses the type-level size information, but makes the full 'Vector'
-- API available.
toVector :: ConstantArray n a -> VS.Vector a
toVector (CA xs) = xs

toList :: Storable a => ConstantArray n a -> [a]
toList (CA v) = VS.toList v

-- | Retrieve the underlying pointer
--
-- Coercible abstraction to look through the `newtype`s wrappers of typedefs.
withPtr ::
     (Coercible b (ConstantArray n a), Storable a)
  => b -> (Ptr a -> IO r) -> IO r
withPtr (coerce -> CA v) k = do
    -- we copy the data, a e.g. int fun(int xs[3]) may mutate it.
    VS.MVector _ fptr <- VS.thaw v
    withForeignPtr fptr k

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

intVal :: forall n. KnownNat n => Proxy n -> Int
intVal p = fromIntegral (natVal p)
