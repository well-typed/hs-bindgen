module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, Positive (..), Small (..), (===), (==>), label, counterexample)

import Foreign.C.Types (CInt)
import Data.Bits (Bits (..))
import HsBindgen.Runtime.Bitfield (Bitfield (..), hiMask, loMask)

main :: IO ()
main = defaultMain $ testGroup "runtime"
    [ testProperty "extend . narrow" extend_narrow_prop
    ]

extend_narrow_prop :: Positive (Small Int) -> CInt -> Property
extend_narrow_prop (Positive (Small w)) x' =
    (w <= 32 && topBit == negative) ==>
    label (if negative then "negative" else "non-negative")
    (counterexample (show (x, negative, topBit))
    (extend (narrow x w) w === x))
  where
    -- we fill or clear the top bits based on the signedness.
    --
    -- this is to make ==> discard less cases (most generated cases otherwise would be discarded)
    x :: CInt
    x = if x' < 0 then x' .|. hiMask w else x' .&. loMask w

    negative = x < 0
    topBit   = testBit x (w - 1)
