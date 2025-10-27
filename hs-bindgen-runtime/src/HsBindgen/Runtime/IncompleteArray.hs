{-# LANGUAGE ViewPatterns #-}
module HsBindgen.Runtime.IncompleteArray (
    IncompleteArray
    -- * Pointers
    -- $pointers
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

import Data.Coerce (Coercible, coerce)
import Data.Vector.Storable qualified as VS
import Foreign.ForeignPtr (mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

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

-- $pointers
--
-- In example C code below, @p1@ points to the array @xs@ as a whole, while @p2@
-- points to the first element of @xs@.
--
-- > extern int xs[];
-- > void foo () {
-- >   int (*p1)[] = &xs;
-- >   int *p2 = &(xs[0]);
-- > }
--
-- Though the types of @p1@ and @p2@ differ, the /values/ of the pointers (the
-- address they point to) is the same. An array is just a block of contiguous
-- memory storing array elements. @p1@ points to where @xs@ starts, and @p2@
-- points to where the first element of @xs@ starts, and these addresses are the
-- same. In Haskell, the corresponding types for @p1@ and @p2@ respectively are
-- @'Ptr' ('IncompleteArray' 'CInt')@ and @'Ptr' 'CInt'@ respectively.
--
-- Functions like 'peekArray' require a @'Ptr' ('IncompleteArray' a)@ argument.
-- If the user only has access to a @'Ptr' a@ but they know that is pointing to
-- the first element in an array, then they can use 'isIncompleteArray' to
-- convert the pointer before using 'peekArray' on it. Conversely, if the user
-- has access to a @'Ptr' ('IncompleteArray' a)@ but they want to convert it to
-- a @'Ptr' a@, then they can use @'isFirstElem'@.
--
-- Relevant functions in this module also support pointers of newtypes around
-- 'IncompleteArray', hence the addition of 'Coercible' constraints in many
-- places. For example, we can use 'isIncompleteArray' at an 'IncompleteArray'
-- type or we can use 'isIncompleteArray' at a newtype around an
-- 'IncompleteArray'.
--
-- > newtype A = A (IncompleteArray CInt)
-- > isIncompleteArray @(IncompleteArray CInt) ::
-- >   Proxy 3 -> Ptr CInt -> Ptr (IncompleteArray CInt)
-- > isIncompleteArray @(A 3) ::
-- >   Proxy 3 -> Ptr CInt -> Ptr (A 3)

-- | Use a pointer to the first element of an array as a pointer to the whole of
-- said array.
--
-- NOTE: this function does not check that the pointer /is/ actually a pointer
-- to the first element of an array.
isIncompleteArray ::
     forall arrayLike a. Coercible arrayLike (IncompleteArray a)
  => Ptr a
  -> Ptr arrayLike
isIncompleteArray = castPtr
  where
    -- The 'Coercible' constraint is unused but that is intentional, so we
    -- circumvent the @-Wredundant-constraints@ warning by defining @_unused@.
    --
    -- Why is it intentional? The constraint adds a little bit of type safety to
    -- the use of 'castPtr', which can normally cast pointers arbitrarily.
    _unused = coerce @arrayLike @(IncompleteArray a)

-- | Use a pointer to a whole array as a pointer to the first element of said
-- array.
isFirstElem ::
     forall arrayLike a. Coercible arrayLike (IncompleteArray a)
  => Ptr arrayLike
  -> Ptr a
isFirstElem ptr = castPtr ptr
  where
    -- The 'Coercible' constraint is unused but that is intentional, so we
    -- circumvent the @-Wredundant-constraints@ warning by defining @_unused@.
    --
    -- Why is it intentional? The constraint adds a little bit of type safety to
    -- the use of 'castPtr', which can normally cast pointers arbitrarily.
    _unused = coerce @arrayLike @(IncompleteArray a)

-- | Peek a number of elements from a pointer to an incomplete array.
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
withPtr ::
     (Coercible b (IncompleteArray a), Storable a)
  => b -> (Ptr a -> IO r) -> IO r
withPtr (coerce -> IA v) k = do
    -- we copy the data, as e.g. @int fun(int xs[])@ may mutate it.
    VS.MVector _ fptr <- VS.thaw v
    withForeignPtr fptr k
