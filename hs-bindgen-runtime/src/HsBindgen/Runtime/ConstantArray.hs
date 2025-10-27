{-# LANGUAGE ViewPatterns #-}
module HsBindgen.Runtime.ConstantArray (
    ConstantArray
    -- * Pointers
    -- $pointers
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
-- first element in an array, then they can use 'isConstantArray' to convert the
-- pointer before using 'peekArray' on it. Conversely, if the user has access to
-- a @'Ptr' ('ConstantArray' n a)@ but they want to convert it to a @'Ptr' a@,
-- then they can use @'isFirstElem'@.
--
-- Relevant functions in this module also support pointers of newtypes around
-- 'ConstantArray', hence the addition of 'Coercible' constraints in many
-- places. For example, we can use 'isConstantArray' at a 'ConstantArray' type
-- or we can use 'isConstantArray' at a newtype around a 'ConstantArray'.
--
-- > newtype A n = A (ConstantArray n CInt)
-- > isConstantArray @(ConstantArray 3 CInt) ::
-- >   Proxy 3 -> Ptr CInt -> Ptr (ConstantArray 3 CInt)
-- > isConstantArray @(A 3) ::
-- >   Proxy 3 -> Ptr CInt -> Ptr (A 3)

-- | Use a pointer to the first element of an array as a pointer to the whole of
-- said array.
--
-- NOTE: this function does not check that the pointer /is/ actually a pointer
-- to the first element of an array.
isConstantArray ::
     forall arrayLike n a. Coercible arrayLike (ConstantArray n a)
  => Proxy n
  -> Ptr a
  -> Ptr arrayLike
isConstantArray _ = castPtr
  where
    -- The 'Coercible' constraint is unused but that is intentional, so we
    -- circumvent the @-Wredundant-constraints@ warning by defining @_unused@.
    --
    -- Why is it intentional? The constraint adds a little bit of type safety to
    -- the use of 'castPtr', which can normally cast pointers arbitrarily.
    _unused = coerce @arrayLike @(ConstantArray n a)

-- | Use a pointer to a whole array as a pointer to the first element of said
-- array.
isFirstElem ::
     forall arrayLike n a. Coercible arrayLike (ConstantArray n a)
  => Ptr arrayLike
  -> (Proxy n, Ptr a)
isFirstElem ptr = (Proxy @n, castPtr ptr)
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
