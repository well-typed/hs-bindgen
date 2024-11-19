-- TODO documentation
module HsBindgen.TestLib.RealFloat (
    -- * Constants
    zero
  , negZero
  , inf
  , negInf
  , nan
  , maxValue
  , minValue
    -- * Conversion
  , floatToWord32
  , floatFromWord32
  , doubleToWord64
  , doubleFromWord64
  ) where

import Data.Word (Word32, Word64)
import Foreign qualified as F
import System.IO.Unsafe (unsafeDupablePerformIO)

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | Zero
zero :: RealFloat a => a
zero = 0
{-# SPECIALIZE zero :: Float #-}
{-# SPECIALIZE zero :: Double #-}

-- | Negative zero
negZero :: RealFloat a => a
negZero = -0
{-# SPECIALIZE negZero :: Float #-}
{-# SPECIALIZE negZero :: Double #-}

-- | Positive infinity
inf :: RealFloat a => a
inf = 1 / 0
{-# SPECIALIZE inf :: Float #-}
{-# SPECIALIZE inf :: Double #-}

-- | Negative infinity
negInf :: RealFloat a => a
negInf = (-1) / 0
{-# SPECIALIZE negInf :: Float #-}
{-# SPECIALIZE negInf :: Double #-}

-- | NaN
nan :: RealFloat a => a
nan = 0 / 0
{-# SPECIALIZE nan :: Float #-}
{-# SPECIALIZE nan :: Double #-}

-- | Maximum (non-infinite) floating point value
maxValue :: forall a. RealFloat a => a
maxValue
    | numDigits == floatDigits @Float  0 = 0xf.fffffp+124
    | numDigits == floatDigits @Double 0 = 0xf.ffffffffffff8p+1020
    | otherwise                          = error "maxValue not float|double"
  where
    numDigits :: Int
    numDigits = floatDigits @a undefined
{-# SPECIALIZE maxValue :: Float #-}
{-# SPECIALIZE maxValue :: Double #-}

-- | Minimum (non-infinite) floating point value
minValue :: RealFloat a => a
minValue = negate maxValue
{-# SPECIALIZE minValue :: Float #-}
{-# SPECIALIZE minValue :: Double #-}

{-------------------------------------------------------------------------------
  Conversion
-------------------------------------------------------------------------------}

floatToWord32 :: Float -> Word32
floatToWord32 = unsafeCast

floatFromWord32 :: Word32 -> Float
floatFromWord32 = unsafeCast

doubleToWord64 :: Double -> Word64
doubleToWord64 = unsafeCast

doubleFromWord64 :: Word64 -> Double
doubleFromWord64 = unsafeCast

unsafeCast :: forall a b. (F.Storable a, F.Storable b) => a -> b
unsafeCast x
    | F.sizeOf x == F.sizeOf y = y
    | otherwise                = error "unsafeCast with different sizes"
  where
    y :: b
    y = unsafeDupablePerformIO . F.alloca $ \ptr -> do
      F.poke ptr x
      F.peek $ F.castPtr ptr
