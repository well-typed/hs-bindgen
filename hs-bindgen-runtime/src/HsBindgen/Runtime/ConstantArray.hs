-- | C arrays of known, constant size
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.ConstantArray qualified as CA
module HsBindgen.Runtime.ConstantArray (
    ConstantArray -- opaque
  , toVector
  , fromVector
    -- * Pointers
    -- $pointers
  , toPtr
  , toFirstElemPtr
  , withPtr
    -- * Construction
  , repeat
  , fromList
    -- * Query
  , toList
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
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)

import HsBindgen.Runtime.Marshal (ReadRaw, StaticSize, WriteRaw)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | A C array of known size
newtype ConstantArray (n :: Nat) a = CA (VS.Vector a)
  deriving stock (Eq, Show)
  deriving anyclass (ReadRaw, StaticSize, WriteRaw)

type role ConstantArray nominal nominal

-- | /( O(1) /): Get the underlying 'VS.Vector' representation
--
-- This makes the full 'VS.Vector' API available.
toVector ::
     forall a n arrayLike.
     Coercible arrayLike (ConstantArray n a)
  => arrayLike
  -> (Proxy n, VS.Vector a)
toVector (coerce -> xs) = (Proxy @n, xs)

-- | /( O(1) /): Construct from a 'VS.Vector' representation
--
-- This makes the full 'VS.Vector' API available.
--
-- Precondition: the vector must have the right number of elements.
fromVector ::
     forall a n arrayLike. (
       Coercible arrayLike (ConstantArray n a)
     , Storable a
     , KnownNat n
     , HasCallStack
     )
  => Proxy n
  -> VS.Vector a
  -> arrayLike
fromVector _ xs
  | VS.length xs == n = coerce xs
  | otherwise = error $ "fromVector: expected " ++ show n ++ " elements"
  where
    n = intVal (Proxy @n)

{-------------------------------------------------------------------------------
  Pointers
-------------------------------------------------------------------------------}

-- $pointers
--
-- In example C code below, @p1@ points to the array @xs@ as a whole, while @p2@
-- points to the first element of @xs@.
--
-- > extern int xs[3];
-- > void foo () {
-- >   int (*p1)[3] = &xs;
-- >   int *p2 = &(xs[0]);
-- > }
--
-- Though the types of @p1@ and @p2@ differ, the /values/ of the pointers (the
-- address they point to) is the same. An array is just a block of contiguous
-- memory storing array elements. @p1@ points to where @xs@ starts, and @p2@
-- points to where the first element of @xs@ starts, and these addresses are the
-- same. In Haskell, the corresponding types for @p1@ and @p2@ respectively are
-- @'Ptr' ('ConstantArray' n 'CInt')@ and @'Ptr' 'CInt'@ respectively.
--
-- Functions like 'peek' require a @'Ptr' ('ConstantArray' n a)@ argument. If
-- the user only has access to a @'Ptr' a@ but they know that is pointing to the
-- first element in an array, then they can use 'toPtr' to convert the
-- pointer before using 'peekArray' on it. Conversely, if the user has access to
-- a @'Ptr' ('ConstantArray' n a)@ but they want to convert it to a @'Ptr' a@,
-- then they can use @'toFirstElemPtr'@.
--
-- NOTE: with overloaded record dot syntax, syntax like @.toFirstElemPtr@ is
-- also supported.
--
-- Relevant functions in this module also support pointers of newtypes around
-- 'ConstantArray', hence the addition of 'Coercible' constraints in many
-- places. For example, we can use 'toPtr' at a 'ConstantArray' type
-- or we can use 'toPtr' at a newtype around a 'ConstantArray'.
--
-- > newtype A n = A (ConstantArray n CInt)
-- > toPtr @(ConstantArray 3 CInt) ::
-- >   Proxy 3 -> Ptr CInt -> Ptr (ConstantArray 3 CInt)
-- > toPtr @(A 3) ::
-- >   Proxy 3 -> Ptr CInt -> Ptr (A 3)

-- | 'toFirstElemPtr' for overloaded record dot syntax
instance HasField "toFirstElemPtr" (Ptr (ConstantArray n a)) (Ptr a) where
  getField = snd . toFirstElemPtr

-- | /( O(1) /): Use a pointer to the first element of an array as a pointer to the whole of
-- said array.
--
-- NOTE: this function does not check that the pointer /is/ actually a pointer
-- to the first element of an array.
toPtr ::
     forall arrayLike n a. Coercible arrayLike (ConstantArray n a)
  => Proxy n
  -> Ptr a
  -> Ptr arrayLike
toPtr _ = castPtr
  where
    -- The 'Coercible' constraint is unused but that is intentional, so we
    -- circumvent the @-Wredundant-constraints@ warning by defining @_unused@.
    --
    -- Why is it intentional? The constraint adds a little bit of type safety to
    -- the use of 'castPtr', which can normally cast pointers arbitrarily.
    _unused = coerce @arrayLike @(ConstantArray n a)

-- | /( O(1) /): Use a pointer to a whole array as a pointer to the first element of said
-- array.
toFirstElemPtr ::
     forall arrayLike n a. Coercible arrayLike (ConstantArray n a)
  => Ptr arrayLike
  -> (Proxy n, Ptr a)
toFirstElemPtr ptr = (Proxy @n, castPtr ptr)
  where
    -- The 'Coercible' constraint is unused but that is intentional, so we
    -- circumvent the @-Wredundant-constraints@ warning by defining @_unused@.
    --
    -- Why is it intentional? The constraint adds a little bit of type safety to
    -- the use of 'castPtr', which can normally cast pointers arbitrarily.
    _unused = coerce @arrayLike @(ConstantArray n a)

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

-- | /( O(n) /): Retrieve the underlying pointer
withPtr ::
     forall b n a r. (Coercible b (ConstantArray n a), Storable a)
  => b -> (Ptr b -> IO r) -> IO r
withPtr (coerce -> CA v) k = do
    -- we copy the data, a e.g. int fun(int xs[3]) may mutate it.
    VS.MVector _ fptr <- VS.thaw v
    withForeignPtr fptr $ \(ptr :: Ptr a) -> k (toPtr (Proxy @n) ptr)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | /( O(n) /)
repeat :: forall n a. (KnownNat n, Storable a) => a -> ConstantArray n a
repeat x = CA (VS.replicate (intVal (Proxy @n)) x)

-- | /( O(n) /): Construct from a list
--
-- Precondition: the list must have the right number of elements.
fromList :: forall n a.
     (KnownNat n, Storable a, HasCallStack)
  => [a] -> ConstantArray n a
fromList xs = fromVector (Proxy @n) (VS.fromList xs)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | /( O(n) /)
toList :: Storable a => ConstantArray n a -> [a]
toList (CA v) = VS.toList v

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

intVal :: forall n. KnownNat n => Proxy n -> Int
intVal p = fromIntegral (natVal p)
