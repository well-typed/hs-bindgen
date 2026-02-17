-- | C arrays of unknown size
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.IncompleteArray qualified as IA
module HsBindgen.Runtime.IncompleteArray (
    IncompleteArray -- opaque
  , toVector
  , fromVector
    -- * Pointers
    -- $pointers
  , toPtr
  , toFirstElemPtr
  , peekArray
  , pokeArray
  , withPtr
    -- * Construction
  , repeat
  , fromList
    -- * Query
  , toList
  ) where

import Prelude hiding (repeat)

import Data.Coerce (Coercible, coerce)
import Data.Vector.Storable qualified as VS
import Foreign.ForeignPtr (mallocForeignPtrArray, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import GHC.Records (HasField (..))

import HsBindgen.Runtime.Array.Class (IsArray (..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A C array of unknown size
newtype IncompleteArray a = IA (VS.Vector a)
  deriving stock (Eq, Show)

type role IncompleteArray nominal

-- | /( O(1) /): Get the underlying 'VS.Vector' representation
--
-- This makes the full 'VS.Vector' API available.
toVector ::
     Coercible arrayLike (IncompleteArray a)
  => arrayLike
  -> VS.Vector a
toVector (coerce -> xs) = xs

-- | /( O(1) /): Construct from a 'VS.Vector' representation
--
-- This makes the full 'VS.Vector' API available.
fromVector ::
     Coercible arrayLike (IncompleteArray a)
  => VS.Vector a
  -> arrayLike
fromVector = coerce

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
-- the first element in an array, then they can use 'toPtr' to convert the
-- pointer before using 'peekArray' on it. Conversely, if the user has access to
-- a @'Ptr' ('IncompleteArray' a)@ but they want to convert it to a @'Ptr' a@,
-- then they can use @'toFirstElemPtr'@.
--
-- NOTE: with overloaded record dot syntax, syntax like @.toFirstElemPtr@ is
-- also supported.
--
-- Relevant functions in this module also support pointers of newtypes around
-- 'IncompleteArray', hence the addition of 'Coercible' constraints in many
-- places. For example, we can use 'toPtr' at an 'IncompleteArray' type or we
-- can use 'toPtr' at a newtype around an 'IncompleteArray'.
--
-- > newtype A = A (IncompleteArray CInt)
-- > toPtr @(IncompleteArray CInt) :: Ptr CInt -> Ptr (IncompleteArray CInt)
-- > toPtr @A                      :: Ptr CInt -> Ptr A

-- | 'toFirstElemPtr' for overloaded record dot syntax
instance HasField "toFirstElemPtr" (Ptr (IncompleteArray a)) (Ptr a) where
  getField = toFirstElemPtr

-- | /( O(1) /): Use a pointer to the first element of an array as a pointer to the whole of
-- said array.
--
-- NOTE: this function does not check that the pointer /is/ actually a pointer
-- to the first element of an array.
toPtr ::
     forall arrayLike a. Coercible arrayLike (IncompleteArray a)
  => Ptr a
  -> Ptr arrayLike
toPtr = castPtr
  where
    -- The 'Coercible' constraint is unused but that is intentional, so we
    -- circumvent the @-Wredundant-constraints@ warning by defining @_unused@.
    --
    -- Why is it intentional? The constraint adds a little bit of type safety to
    -- the use of 'castPtr', which can normally cast pointers arbitrarily.
    _unused = coerce @arrayLike @(IncompleteArray a)

-- | /( O(1) /): Use a pointer to a whole array as a pointer to the first element of said
-- array.
toFirstElemPtr ::
     forall arrayLike a. Coercible arrayLike (IncompleteArray a)
  => Ptr arrayLike
  -> Ptr a
toFirstElemPtr ptr = castPtr ptr
  where
    -- The 'Coercible' constraint is unused but that is intentional, so we
    -- circumvent the @-Wredundant-constraints@ warning by defining @_unused@.
    --
    -- Why is it intentional? The constraint adds a little bit of type safety to
    -- the use of 'castPtr', which can normally cast pointers arbitrarily.
    _unused = coerce @arrayLike @(IncompleteArray a)

-- | /( O(n) /): Peek a number of elements from a pointer to an incomplete array.
peekArray ::
     forall a arrayLike. (Coercible arrayLike (IncompleteArray a), Storable a)
  => Int
  -> Ptr arrayLike
  -> IO arrayLike
peekArray = peekArrayOff 0

-- | /( O(n) /): Peek a number of elements from a pointer to an incomplete array, starting
-- at an offset in terms of a number of array elements into the array pointer.
peekArrayOff ::
     forall a arrayLike. (Coercible arrayLike (IncompleteArray a), Storable a)
  => Int
  -> Int
  -> Ptr arrayLike
  -> IO arrayLike
peekArrayOff off size ptr = do
    fptr <- mallocForeignPtrArray @a size
    withForeignPtr fptr $ \(ptr' :: Ptr a) -> do
        copyBytes ptr' (castPtr ptr `plusPtr` offBytes) (size * sizeOfA)
    vs <- VS.freeze (VS.MVector size fptr)
    return $ coerce (IA vs)
  where
    sizeOfA = sizeOf (undefined :: a)
    offBytes = sizeOfA * off

-- | /( O(n) /): Poke a number of elements to a pointer to an incomplete array.
pokeArray ::
     forall a arrayLike. (Coercible arrayLike (IncompleteArray a), Storable a)
  => Ptr arrayLike
  -> arrayLike
  -> IO ()
pokeArray = pokeArrayOff 0

-- | /( O(n) /): Poke a number of elements to a pointer to an incomplete array, starting at
-- an offset in terms of a number of array elements into the array pointer.
pokeArrayOff ::
     forall a arrayLike. (Coercible arrayLike (IncompleteArray a), Storable a)
  => Int
  -> Ptr arrayLike
  -> arrayLike
  -> IO ()
pokeArrayOff off ptr (coerce -> IA vs) = do
    VS.MVector size fptr <- VS.unsafeThaw vs
    withForeignPtr fptr $ \(ptr' :: Ptr a) ->
      copyBytes (castPtr ptr) (ptr' `plusPtr` offBytes) (size * sizeOfA)
  where
    sizeOfA = sizeOf (undefined :: a)
    offBytes = sizeOfA * off

-- | /( O(n) /): Retrieve the underlying pointer
withPtr ::
     (Coercible b (IncompleteArray a), Storable a)
  => b -> (Ptr b -> IO r) -> IO r
withPtr (coerce -> IA v) k = do
    -- we copy the data, as e.g. @int fun(int xs[])@ may mutate it.
    VS.MVector _ fptr <- VS.thaw v
    withForeignPtr fptr $ \(ptr :: Ptr a) -> k (toPtr ptr)

instance Storable a => IsArray (IncompleteArray a) where
  type Elem (IncompleteArray a) = a
  -- | /( O(n) /)
  withElemPtr (IA v) k = do
    -- we copy the data, as e.g. @int fun(int xs[])@ may mutate it.
    VS.MVector _ fptr <- VS.thaw v
    withForeignPtr fptr $ \(ptr :: Ptr a) -> k ptr

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | /( O(n) /)
repeat :: Storable a => Int -> a -> IncompleteArray a
repeat n x = IA (VS.replicate n x)

-- | /( O(n) /)
fromList :: Storable a => [a] -> IncompleteArray a
fromList xs = IA (VS.fromList xs)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | /( O(n) /)
toList :: Storable a => IncompleteArray a -> [a]
toList (IA v) = VS.toList v
