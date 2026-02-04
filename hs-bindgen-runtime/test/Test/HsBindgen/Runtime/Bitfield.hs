module Test.HsBindgen.Runtime.Bitfield (tests) where

import Data.Bits (Bits (..))
import Data.Word (Word64)
import Foreign (Ptr)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Test.QuickCheck (NonNegative (..), Positive (..), Property, Small (..),
                        counterexample, ioProperty, label, (===), (==>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import HsBindgen.Runtime.Internal.Bitfield (Bitfield (..), hiMask, loMask,
                                            peekBitOffWidth, pokeBitOffWidth)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.Runtime.Internal.Bitfield" [
      testProperty "extend . narrow" extend_narrow_prop
    , testProperty "bit peek . poke" bit_peek_poke_prop
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

bit_peek_poke_prop_impl
    :: (Int -> Int -> Int -> Ptr a -> Word64 -> IO Property)
    -> NonNegative (Small Int) -> Positive (Small Int) -> Word64 -> Property
bit_peek_poke_prop_impl test (NonNegative (Small off)) (Positive (Small (mod64 -> width))) x =
    label l $ mod off 64 + width <= 64 ==> ioProperty prop
  where
    l | mod off 8  + width <=  8 = "uint8"
      | mod off 16 + width <= 16 = "uint16"
      | mod off 32 + width <= 32 = "uint32"
      | mod off 64 + width <= 64 = "uint64"
      | otherwise                = "panic"

    size = div (off + width) 8 + 1
    prop = allocaBytes size $ \ptr -> do
        fillBytes ptr 0xAA size
        test off width size ptr x

bit_peek_poke_prop :: NonNegative (Small Int) -> Positive (Small Int) -> Word64 -> Property
bit_peek_poke_prop = bit_peek_poke_prop_impl $ \off width _size ptr x -> do
    pokeBitOffWidth ptr off width x
    x' <- peekBitOffWidth ptr off width

    return $ narrow x width === x'

mod64 :: Integral a => a -> a
mod64 x = mod x 64
