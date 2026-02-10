module Test.Util (
    showPowersOf10
  , showPowersOf
  , showBucketsOf
  ) where

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Printf

showPowersOf10 :: Int -> String
showPowersOf10 = showPowersOf 10

showPowersOf :: Int -> Int -> String
showPowersOf factor n
  | factor <= 1 = error "showPowersOf: factor must be larger than 1"
  | n < 0       = "n < 0"
  | n == 0      = "n == 0"
  | otherwise   = printf "%d <= n < %d" lb ub
  where
    ub = fromJust (find (n <) (iterate (* factor) factor))
    lb = ub `div` factor

showBucketsOf :: Int -> Int -> String
showBucketsOf range n
  | range <= 0 = error "showBucketsOf: range must be larger than 0"
  | n == 0     = "n == 0"
  | m == 0     = printf "%d < n < %d" lb ub
  | otherwise  = printf "%d <= n < %d" lb ub
  where
    m = n `div` range
    lb = m * range
    ub = (m + 1) * range
