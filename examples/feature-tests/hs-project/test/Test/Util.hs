module Test.Util (
    -- * Show
    showPowersOf10
  , showPowersOf
  , showBucketsOf
    -- * Type-level programming
  , type ($)
    -- * Static properties
  , dictProperty
  ) where

import Control.Exception (Exception (displayException), SomeException, evaluate,
                          try)
import Data.Constraint (Dict (Dict))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (fromJust)
import Test.Tasty.QuickCheck (Property, Testable (property), counterexample,
                              ioProperty, once)
import Text.Printf (printf)

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Type-level programming
-------------------------------------------------------------------------------}

type family ($) f x where
  f $ x = f x

{-------------------------------------------------------------------------------
  Static properties
-------------------------------------------------------------------------------}

-- | Test evidence of a constraint
--
-- 'Dict'ionaries are not testable in the convential sense using QuickCheck
-- properties. Dictionaries carry static information that is checked (or in
-- other words, "tested") by the compiler. However, since Haskell is a lazy
-- language, we should still check dynamically that the dictionaries are not
-- [bottom](https://wiki.haskell.org/Bottom) values. 'dictProperty' performs the
-- "bottom" check, while the compiler takes care of the rest.
dictProperty :: Dict c -> Property
dictProperty d = once $ ioProperty @Property $ do
    try @SomeException (evaluate d) <&> \case
      Left e -> counterexample (displayException e) False
      Right Dict -> property True
