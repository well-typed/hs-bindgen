-- TODO documentation
module HsBindgen.TestLib.Transform (
    -- * Transform
    Transform(..)
  ) where

import Data.Bits qualified as Bits
import Data.Word (Word32, Word64)
import Foreign.C.Types qualified as FC

import HsBindgen.TestLib.RealFloat qualified as RF

{-------------------------------------------------------------------------------
  Transform
-------------------------------------------------------------------------------}

-- | Transform a value to a different value
--
-- prop> not (transform x `repEq` x)
class Transform a where
  transform :: a -> a

instance Transform FC.CChar where
  transform = (+ 1)

instance Transform FC.CInt where
  transform = (+ 1)

instance Transform FC.CFloat where
  transform (FC.CFloat x) = FC.CFloat $ transformFloat x

instance Transform FC.CDouble where
  transform (FC.CDouble x) = FC.CDouble $ transformDouble x

{-------------------------------------------------------------------------------
  Auxilliary functions
-------------------------------------------------------------------------------}

-- | Transform a 'Float' value
--
-- * NaN is transformed to negative zero
-- * Negative zero is transformed to NaN
-- * Infinity is transformed to negative infinity
-- * Negative infinity is transformed to infinity
-- * Denormalized values are cycled: from zero to maximum denormalized value,
--   then from smallest denormalized negative value to minimum denormalized
--   value, then from zero again
-- * Normalized values are cycled: fraction first, exponent second, sign third
transformFloat :: Float -> Float
transformFloat x
    | isNaN x          = RF.negZero
    | isNegativeZero x = RF.nan
    | isInfinite x     = negate x
    | isDenormalized x = RF.floatFromWord32 wRD
    | otherwise        = RF.floatFromWord32 wRN
  where
    -- Value as a word to transform at the representation level
    w :: Word32
    w = RF.floatToWord32 x

    -- Index of exponent (least significant bit)
    --
    -- 3  2          1         0
    -- 1 09876543 21098765432109876543210
    -- s ---e8--- ----------f23----------
    eIdx :: Int
    eIdx = 23

    -- Sign bit mask (not shifted), exponent mask (shifted), fraction mask
    sMask, eMask, fMask :: Word32
    sMask = 0x80000000
    eMask = 0xff
    fMask = 0x7fffff

    -- Sign bit (not shifted), complement (not shifted)
    s, sC :: Word32
    s = w Bits..&. sMask
    sC = Bits.xor s sMask

    -- Max non-infinite exponent, min non-zero exponent, exponent, incremented
    -- exponent
    --
    -- * All zero bits: denormalized form
    -- * All one bits: infinite or NaN
    eMax, eMin, e, e1 :: Word32
    eMax = eMask - 1
    eMin = fMask + 1
    e = Bits.shiftR w eIdx Bits..&. eMask
    e1 = e + 1

    -- Max fraction, fraction, incremented fraction
    fMax, f, f1 :: Word32
    fMax = fMask
    f = w Bits..&. fMask
    f1 = f + 1

    -- Denormalized word result
    --
    -- 1. If still room in fraction bits, return incremented
    -- 2. If overflow positive, return smallest negative value
    -- 3. If overflow negative, return zero
    wRD :: Word32
    wRD
      | f1 <= fMax = w + 1
      | s == 0     = sMask + 1
      | otherwise  = 0

    -- Normal word result
    --
    -- 1. If still room in fraction bits, return incremented
    -- 2. If still room in exponent bits, return
    --    sign + incremented exponent + zero fraction
    -- 3. Return complemented sign, smallest exponent, zero fraction
    wRN :: Word32
    wRN
      | f1 <= fMax = w + 1
      | e1 <= eMax = s + Bits.shiftL e1 eIdx
      | otherwise  = sC + eMin

-- | Transform a 'Double' value
--
-- * NaN is transformed to negative zero
-- * Negative zero is transformed to NaN
-- * Infinity is transformed to negative infinity
-- * Negative infinity is transformed to infinity
-- * Denormalized values are cycled: from zero to maximum denormalized value,
--   then from smallest denormalized negative value to minimum denormalized
--   value, then from zero again
-- * Normalized values are cycled: fraction first, exponent second, sign third
transformDouble :: Double -> Double
transformDouble x
    | isNaN x          = RF.negZero
    | isNegativeZero x = RF.nan
    | isInfinite x     = negate x
    | isDenormalized x = RF.doubleFromWord64 wRD
    | otherwise        = RF.doubleFromWord64 wRN
  where
    -- Value as a word to transform at the representation level
    w :: Word64
    w = RF.doubleToWord64 x

    -- Index of exponent (least significant bit)
    --
    -- 6    5          4         3         2         1         0
    -- 3 21098765432 1098765432109876543210987654321098765432109876543210
    -- s ----e11---- ------------------------f52-------------------------
    eIdx :: Int
    eIdx = 52

    -- Sign bit mask (not shifted), exponent mask (shifted), fraction mask
    sMask, eMask, fMask :: Word64
    sMask = 0x8000000000000000
    eMask = 0x7ff
    fMask = 0xfffffffffffff

    -- Sign bit (not shifted), complement (not shifted)
    s, sC :: Word64
    s = w Bits..&. sMask
    sC = Bits.xor s sMask

    -- Max non-infinite exponent, min non-zero exponent, exponent, incremented
    -- exponent
    --
    -- * All zero bits: denormalized form
    -- * All one bits: infinite or NaN
    eMax, eMin, e, e1 :: Word64
    eMax = eMask - 1
    eMin = fMask + 1
    e = Bits.shiftR w eIdx Bits..&. eMask
    e1 = e + 1

    -- Max fraction, fraction, incremented fraction
    fMax, f, f1 :: Word64
    fMax = fMask
    f = w Bits..&. fMask
    f1 = f + 1

    -- Denormalized word result
    --
    -- 1. If still room in fraction bits, return incremented
    -- 2. If overflow positive, return smallest negative value
    -- 3. If overflow negative, return zero
    wRD :: Word64
    wRD
      | f1 <= fMax = w + 1
      | s == 0     = sMask + 1
      | otherwise  = 0

    -- Normal word result
    --
    -- 1. If still room in fraction bits, return incremented
    -- 2. If still room in exponent bits, return
    --    sign + incremented exponent + zero fraction
    -- 3. Return complemented sign, smallest exponent, zero fraction
    wRN :: Word64
    wRN
      | f1 <= fMax = w + 1
      | e1 <= eMax = s + Bits.shiftL e1 eIdx
      | otherwise  = sC + eMin
