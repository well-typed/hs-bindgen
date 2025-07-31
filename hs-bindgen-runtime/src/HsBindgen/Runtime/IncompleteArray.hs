{-# LANGUAGE ViewPatterns #-}
module HsBindgen.Runtime.IncompleteArray (
    IncompleteArray
    -- * Pointers
  , isIncompleteArray
  , isFirstElem
    -- ** Peek and poke
  , peekArray
  , pokeArray
    -- * Construction
  , repeat
  , fromList
    -- * Query
  , toVector
  , toList
  , withPtr
  ) where

import Prelude hiding (repeat)

import Data.Vector.Storable qualified as VS
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtrArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))
import Data.Coerce (Coercible, coerce)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A C array of unknown size
newtype IncompleteArray a = IA (VS.Vector a)
  deriving stock (Eq, Show)

type role IncompleteArray nominal

{-------------------------------------------------------------------------------
  Pointers
-------------------------------------------------------------------------------}

-- | Use a pointer to the first element of an array as a pointer to the whole of
-- said array.
--
-- The coercible abstraction is here so that the function can see through
-- @newtype@ wrappers of @typedefs@.
isIncompleteArray ::
     forall arrayLike a. Coercible arrayLike (IncompleteArray a)
  => Ptr a
  -> Ptr arrayLike
isIncompleteArray = castPtr

-- | Use a pointer to a whole array as a pointer to the first element of said
-- array.
--
-- The coercible constraint is here so that the function can see through
-- @newtype@ wrappers of @typedefs@.
isFirstElem ::
     forall arrayLike a. Coercible arrayLike (IncompleteArray a)
  => Ptr arrayLike
  -> Ptr a
isFirstElem ptr = castPtr ptr

-- | Peek a number of elements from a pointer to an incomplete array.
--
-- The coercible constraint is here so that the function can see through
-- @newtype@ wrappers of @typedefs@.
peekArray ::
     forall a arrayLike. (Coercible arrayLike (IncompleteArray a), Storable a)
  => Int
  -> Ptr arrayLike
  -> IO arrayLike
peekArray size ptr = do
    fptr <- mallocForeignPtrArray @a size
    withForeignPtr fptr $ \ptr' -> do
        copyBytes ptr' (castPtr ptr) (size * sizeOfA)
    vs <- VS.freeze (VS.MVector size fptr)
    return $ coerce (IA vs)
  where
    sizeOfA = sizeOf (undefined :: a)

-- | Poke a number of elements to a pointer to an incomplete array.
--
-- The coercible constraint is here so that the function can see through
-- @newtype@ wrappers of @typedefs@.
pokeArray ::
     forall a arrayLike. (Coercible arrayLike (IncompleteArray a), Storable a)
  => Ptr arrayLike
  -> arrayLike
  -> IO ()
pokeArray ptr (coerce -> IA vs) = do
    VS.MVector size fptr <- VS.unsafeThaw vs
    withForeignPtr fptr $ \ptr' ->
      copyBytes ptr (castPtr ptr') (size * sizeOfA)
  where
    sizeOfA = sizeOf (undefined :: a)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

repeat :: Storable a => Int -> a -> IncompleteArray a
repeat n x = IA (VS.replicate n x)

fromList :: Storable a => [a] -> IncompleteArray a
fromList xs = IA (VS.fromList xs)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | /( O(1) /): get the underlying 'VS.Vector' representation
--
-- This makes the full 'VS.Vector' API available.
toVector :: IncompleteArray a -> VS.Vector a
toVector (IA xs) = xs

-- | /( O(n) /)
toList :: Storable a => IncompleteArray a -> [a]
toList (IA v) = VS.toList v

-- | Retrieve the underlying pointer
--
-- Coercible abstraction to look through the @newtype@ wrappers of typedefs.
withPtr ::
     (Coercible b (IncompleteArray a), Storable a)
  => b -> (Ptr a -> IO r) -> IO r
withPtr (coerce -> IA v) k = do
    -- we copy the data, as e.g. @int fun(int xs[3])@ may mutate it.
    VS.MVector _ fptr <- VS.thaw v
    withForeignPtr fptr k
