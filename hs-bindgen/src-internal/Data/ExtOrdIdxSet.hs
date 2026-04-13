-- | Set of integral indices ordered by a comparison function for associated
-- values
--
-- Intended for qualified import.
--
-- > import Data.ExtOrdIdxSet (ExtOrdIdxSet)
-- > import Data.ExtOrdIdxSet qualified as ExtOrdIdxSet
module Data.ExtOrdIdxSet (
    -- * Type
    ExtOrdIdxSet -- opaque
    -- * Construction
  , fromIntMap
    -- * Insertion
  , insert
    -- * Combine
  , (\\)
    -- * Min/Max
  , minView
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Ord qualified as Ord
import Data.Set (Set)
import Data.Set qualified as Set

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

-- | Internal set element type
--
-- The associated value and comparison function are included in order to
-- implement the 'Ord' instance.  All elements of the set have the same
-- comparison function.
data Ele a = Ele {
      idx :: Int
    , val :: a
    , cmp :: a -> a -> Ordering
    }

instance Eq (Ele a) where
  l == r = l.cmp l.val r.val == Ord.EQ

instance Ord (Ele a) where
  compare l r = l.cmp l.val r.val

instance Show a => Show (Ele a) where
  show e = unwords ["Ele", show e.idx, show e.val, "cmp"]

-- | Set of integral indices ordered by a comparison function for associated
-- values
--
-- This is an /opaque/ type that wraps a 'Set' of elements ordered by a
-- comparison function.  It also contains the comparison function itself, used
-- when inserting new elements.
--
-- For a given comparison function, values that compare equally must correspond
-- to equal indices.
data ExtOrdIdxSet a = ExtOrdIdxSet {
      set :: Set (Ele a)
    , cmp :: a -> a -> Ordering
    }

instance Show a => Show (ExtOrdIdxSet a) where
  show s = "ExtOrdIdxSet{ set = " ++ show s.set ++ ", cmp }"

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Construct a set from an 'IntMap' with the given comparison function
fromIntMap :: forall a. (a -> a -> Ordering) -> IntMap a -> ExtOrdIdxSet a
fromIntMap cmp m = ExtOrdIdxSet{
      set = IntMap.foldrWithKey' aux Set.empty m
    , cmp = cmp
    }
  where
    aux :: Int -> a -> Set (Ele a) -> Set (Ele a)
    aux idx val = Set.insert (Ele idx val cmp)

{-------------------------------------------------------------------------------
  Insertion
-------------------------------------------------------------------------------}

-- | Insert an element into the set
insert :: Int -> a -> ExtOrdIdxSet a -> ExtOrdIdxSet a
insert idx val s = s{ set = Set.insert (Ele idx val s.cmp) s.set }

{-------------------------------------------------------------------------------
  Combine
-------------------------------------------------------------------------------}

-- | Remove elements of an 'IntSet' from a set
--
-- This is done using the index, /not/ the comparison function.
(\\) :: forall a. ExtOrdIdxSet a -> IntSet -> ExtOrdIdxSet a
l \\ r = l{ set = Set.filter aux l.set }
  where
    aux :: Ele a -> Bool
    aux e = IntSet.notMember e.idx r

{-------------------------------------------------------------------------------
  Min/Max
-------------------------------------------------------------------------------}

-- | Get the minimal element of a set and the set stripped of that element
--
-- This function returns 'Nothing' when the set is empty.
minView :: ExtOrdIdxSet a -> Maybe (Int, ExtOrdIdxSet a)
minView s = case Set.minView s.set of
    Just (e, s') -> Just (e.idx, s{ set = s' })
    Nothing      -> Nothing
