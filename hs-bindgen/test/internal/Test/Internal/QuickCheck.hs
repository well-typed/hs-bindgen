module Test.Internal.QuickCheck (
    -- * Generators
    frequency_
  , vectorOfU
  , vectorOfS
  , vectorOfF
  ) where

import Test.QuickCheck

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | Generates one of the given values, with a weighted random distribution
--
-- The input list must be non-empty.
frequency_ :: [(Int, a)] -> Gen a
frequency_ = frequency . map (fmap pure)

-- | Generate a list of length determined by a uniform random distribution over
-- the specified bounds
vectorOfU :: Int -> Int -> Gen a -> Gen [a]
vectorOfU minB maxB gen = do
    n <- choose (minB, minB `max` maxB)
    vectorOf n gen

-- | Generate a list of length determined by a uniform random distribution over
-- the specified bounds, where the maximum bound depends on the size parameter
--
-- Inspired by @Cabal-QuickCheck@
vectorOfS :: Int -> Int -> Gen a -> Gen [a]
vectorOfS minB maxB gen = sized $ \size -> do
    n <- choose (minB, minB `max` ((size `div` 2) `min` maxB))
    vectorOf n gen

-- | Generate a list of length determined by a weighted random distribution
--
-- The input list must be non-empty.
vectorOfF :: [(Int, Int)] -> Gen a -> Gen [a]
vectorOfF ns gen = do
    n <- frequency_ ns
    vectorOf n gen
