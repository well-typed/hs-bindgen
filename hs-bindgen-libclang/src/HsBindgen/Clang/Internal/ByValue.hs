{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Dealing with structs-by-value
module HsBindgen.Clang.Internal.ByValue (
    OnHaskellHeap(..)
    -- * Construction
  , HasKnownSize(..)
  , copyToHaskellHeap
    -- * Access
  , R(..)
  , LivesOnHaskellHeap -- opaque
  , onHaskellHeap
    -- * Preallocation
  , W(..)
  , Preallocate(..)
  , preallocate_
  , preallocatePair
  , preallocatePair_
  ) where

import Foreign
import GHC.Exts
import GHC.IO
import Data.Array.Byte (ByteArray (..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data OnHaskellHeap tag = OnHaskellHeap ByteArray#

instance Eq (OnHaskellHeap tag) where
  OnHaskellHeap a == OnHaskellHeap b = ByteArray a == ByteArray b

instance Ord (OnHaskellHeap tag) where
  compare (OnHaskellHeap a) (OnHaskellHeap b) = compare (ByteArray a) (ByteArray b)

instance Show (OnHaskellHeap tag) where
  showsPrec d (OnHaskellHeap a) = showsPrec d (ByteArray a)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Structs with known size
--
-- Intended for use with a type argument:
--
-- > knownSize @CXToken_
class HasKnownSize tag where
  knownSize :: Int

copyToHaskellHeap :: forall tag.
     HasKnownSize tag
  => Ptr tag -> IO (OnHaskellHeap tag)
copyToHaskellHeap src = fmap fst $
    mkByteArray# (knownSize @tag) OnHaskellHeap $ \arr -> do
      let dest :: Ptr tag
          dest = Ptr (mutableByteArrayContents# arr)
      copyBytes dest src (knownSize @tag)

{-------------------------------------------------------------------------------
  Access
-------------------------------------------------------------------------------}

newtype R tag = R ByteArray#

-- | Heap-allocated structs
--
-- The definition of this class is not exported; instances are expected to be
-- derived using newtype deriving.
class LivesOnHaskellHeap a where
  type Reading a :: UnliftedType

  -- | Get a pointer to a heap-allocated struct
  --
  -- This essentially just unwraps the lifted 'OnHaskellHeap' type, but then
  -- rewrapping it as the unlifted 'R' newtype, to avoid losing type info.
  onHaskellHeap :: a -> (Reading a -> IO r) -> IO r

instance LivesOnHaskellHeap (OnHaskellHeap tag) where
  type Reading (OnHaskellHeap tag) = R tag
  onHaskellHeap (OnHaskellHeap arr) f = f (R arr)

{-------------------------------------------------------------------------------
  Preallocation
-------------------------------------------------------------------------------}

newtype W tag = W (MutableByteArray# RealWorld)

-- | Preallocate a buffer
--
-- The definition of this class is not exported; instances are expected to be
-- derived through newtype deriving.
class Preallocate a where
  type Writing a :: UnliftedType

  -- | Preallocate a buffer
  --
  -- See 'onHaskellHeap' for rationale.
  preallocate :: (Writing a -> IO r) -> IO (a, r)

preallocate_ :: Preallocate  a => (Writing a -> IO ()) -> IO a
preallocate_ = fmap fst . preallocate

instance HasKnownSize tag => Preallocate (OnHaskellHeap tag) where
  type Writing (OnHaskellHeap tag) = W tag

  preallocate :: (W tag -> IO r) -> IO (OnHaskellHeap tag, r)
  preallocate f =
      mkByteArray# (knownSize @tag) OnHaskellHeap $ \arr ->
        f (W arr)

-- | Preallocate two values
--
-- TODO: It would be nice to generalize this, but I can't quite figure out how
-- without introducing a ton of machinery.
preallocatePair :: forall a b r.
     (Preallocate a, Preallocate b)
  => (Writing a -> Writing b -> IO r)
  -> IO ((a, b), r)
preallocatePair k = fmap reassoc $
    preallocate $ \wa ->
    preallocate $ \wb ->
      k wa wb
  where
    reassoc :: (a, (b, r)) -> ((a, b), r)
    reassoc (a, (b, r)) = ((a, b), r)

preallocatePair_ ::
     (Preallocate a, Preallocate b)
  => (Writing a -> Writing b -> IO ())
  -> IO (a, b)
preallocatePair_ = fmap fst . preallocatePair

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

mkByteArray# ::
     Int
  -> (ByteArray# -> a)
  -> (MutableByteArray# RealWorld -> IO b)
  -> IO (a, b)
mkByteArray# (I# sz) wrap fill = IO $ \w0 ->
    let !(# w1, arr  #) = newPinnedByteArray# sz     w0
        !(# w2, b    #) = unIO (fill arr)            w1
        !(# w3, arr' #) = unsafeFreezeByteArray# arr w2
    in (# w3, (wrap arr', b) #)
