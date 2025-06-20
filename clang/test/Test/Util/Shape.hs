-- | Working with trees of specific shapes
--
-- Intended for qualified import.
--
-- > import Test.Util.Shape (Shape)
-- > import Test.Util.Shape qualified as Shape
module Test.Util.Shape (
    Shape -- opaque
    -- * Relate trees and shapes
  , toForest
  , toForest_
  , fromForest
  ) where

import Control.Monad
import Data.List qualified as List
import Data.Tree (Forest, Tree(Node))
import Data.Tree qualified as Tree
import Test.QuickCheck

{-------------------------------------------------------------------------------
  Internal auxiliary: shape of a tree
-------------------------------------------------------------------------------}

-- | Tree shape
--
-- We refer to this as the /shape/ of a tree because although we can have
-- annotations in the tree (of type @a@, generation and shrinking of these
-- values is entirely independent from generation and shrinking of the
-- shape of the tree.
newtype Shape a = Shape (Forest a)

instance Show a => Show (Shape a) where
  show = Tree.drawTree . Node "" . map (fmap show) . toForest

instance Arbitrary1 Shape where
  liftArbitrary :: forall a. Gen a -> Gen (Shape a)
  liftArbitrary f = sized $ \n -> do
      maxDepth <- choose (0, 5)

      let go :: Int -> Int -> Gen (Forest a)
          go curDepth numElems | curDepth == maxDepth =
              replicateM numElems $ Node <$> f <*> pure []
          go curDepth numElems = do
              partitioned <- arbitraryPartitioning maxNumChildren numElems
              forM partitioned $ \numInPart ->
                Node <$> f <*> go (curDepth + 1) (numInPart - 1)
            where
              maxNumChildren = 3

      Shape <$> go 0 n

  liftShrink :: forall a. (a -> [a]) -> Shape a -> [Shape a]
  liftShrink f (Shape forest) = map Shape $ shrinkList (liftShrink f) forest

{-------------------------------------------------------------------------------
  Relate trees and shapes
-------------------------------------------------------------------------------}

toForest :: Shape a -> Forest (a, [Int])
toForest = \(Shape forest) -> goForest [] forest
  where
    goForest :: [Int] -> Forest a -> Forest (a, [Int])
    goForest path = zipWith (goTree path) [0..]

    goTree :: [Int] -> Int -> Tree a -> Tree (a, [Int])
    goTree path i (Node x xs) = Node (x, path') (goForest path' xs)
      where
        path' :: [Int]
        path' = path ++ [i]

toForest_ :: Shape () -> Forest [Int]
toForest_ = map (fmap snd) . toForest

fromForest :: Forest a -> Shape a
fromForest = Shape

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Divide @n@ elements into random but non-empty buckets
--
-- For example, @arbitraryPartitioning 3 10@ might result in
--
-- > [1,3,6]
-- > [1,4,5]
-- > [1,8,1]
-- > [1,8,1]
-- > [10]
-- > [2,1,7]
-- > [2,8]
-- > [7,3]
arbitraryPartitioning :: Int -> Int -> Gen [Int]
arbitraryPartitioning _ 0 =
    return []
arbitraryPartitioning 0 _ =
    error "must have at least one partition"
arbitraryPartitioning maxNumParts numElems = do
    -- Don't choose more partitions than we have parts
    numParts <- choose (1, min maxNumParts numElems)
    offsets  <-
      -- We only compute any offset if we have at least two partitions. Since we
      -- cannot have more partitions than elements, this also means that we must
      -- have at least two elements. Therefore this choice is well-defined.
                 (replicateM (numParts - 1) $ choose (1, numElems - 1))
      -- Duplicates would result in empty partitions.
      `suchThat` noDups
    return $ partitionSizes numElems offsets

-- | Compute partition sizes from partition offsets
--
-- The resulting list reports the number of elements in each partition.
--
-- For example:
--
-- > partitionSizes 10 []      == [10]    -- single partition
-- > partitionSizes 10 [0]     == [0,10]  -- two partitions, first empty
-- > partitionSizes 10 [1]     == [1,9]
-- > partitionSizes 10 [2]     == [2,8]
-- > partitionSizes 10 [2,2]   == [2,0,8] -- second partition empty
-- > partitionSizes 10 [2,3]   == [2,1,7]
-- > partitionSizes 10 [2,4]   == [2,2,6]
-- > partitionSizes 10 [2,4,9] == [2,2,5,1]
partitionSizes :: Int -> [Int] -> [Int]
partitionSizes numElems =
    go 0 . List.sort
  where
    go :: Int -> [Int] -> [Int]
    go prev []     = [numElems - prev]
    go prev (o:os) = o - prev : go o os

-- | Check that a list does not contain any duplicates
noDups :: Eq a => [a] -> Bool
noDups xs = length xs == length (List.nub xs)
