{-# LANGUAGE ViewPatterns #-}
module HsBindgen.Runtime.ConstantArray (
    ConstantArray
    -- * Pointers
  , isConstantArray
  , isFirstElem
    -- * Construction
  , repeat
  , fromList
    -- * Query
  , toVector
  , toList
  , withPtr
    -- * Auxiliary
  , intVal
  ) where

import Prelude hiding (repeat)

import Data.Coerce (Coercible, coerce)
import Data.Proxy (Proxy (..))
import Data.Vector.Storable qualified as VS
import Foreign.ForeignPtr (mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.Stack
import GHC.TypeNats (KnownNat, Nat, natVal)

import HsBindgen.Runtime.Marshal (ReadRaw, StaticSize, WriteRaw)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A C array of known size
newtype ConstantArray (n :: Nat) a = CA (VS.Vector a)
  deriving (Eq, Show)
  deriving anyclass (ReadRaw, StaticSize, WriteRaw)

type role ConstantArray nominal nominal

{-------------------------------------------------------------------------------
  Pointers
-------------------------------------------------------------------------------}

-- | Use a pointer to the first element of an array as a pointer to the whole of
-- said array.
--
-- The coercible abstraction is here so that the function can see through
-- @newtype@ wrappers of @typedefs@.
isConstantArray ::
     forall arrayLike n a. Coercible arrayLike (ConstantArray n a)
  => Proxy n
  -> Ptr a
  -> Ptr arrayLike
isConstantArray _ = castPtr

-- | Use a pointer to a whole array as a pointer to the first element of said
-- array.
--
-- The coercible constraint is here so that the function can see through
-- @newtype@ wrappers of @typedefs@.
isFirstElem ::
     forall arrayLike n a. Coercible arrayLike (ConstantArray n a)
  => Ptr arrayLike
  -> (Proxy n, Ptr a)
isFirstElem ptr = (Proxy @n, castPtr ptr)

instance (Storable a, KnownNat n) => Storable (ConstantArray n a) where
    sizeOf _ = intVal (Proxy @n) * sizeOf (undefined :: a)

    alignment _ = alignment (undefined :: a)

    peek ptr = do
        fptr <- mallocForeignPtrArray size
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
  Auxiliary
-------------------------------------------------------------------------------}

intVal :: forall n. KnownNat n => Proxy n -> Int
intVal p = fromIntegral (natVal p)
