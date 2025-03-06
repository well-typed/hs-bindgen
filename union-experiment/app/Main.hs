module Main where

import Test.Tasty
import Test.Tasty.QuickCheck

import Example.Hs qualified as Hs
import Example.C  qualified as C

main :: IO ()
main = defaultMain $ testGroup "union-experiment" [
      testGroup "hs-vs-c" [
          testProperty "max"  prop_CHs_max
        , testProperty "grow" prop_CHs_grow
        ]
    ]

prop_CHs_max :: Hs.Dim -> Property
prop_CHs_max dim =
        Hs.max dim
    === (fromIntegral . C.max . Hs.toC $ dim)

prop_CHs_grow :: Hs.Dim -> Property
prop_CHs_grow dim =
        Hs.grow dim
    === (Hs.fromC . C.grow . Hs.toC $ dim)
