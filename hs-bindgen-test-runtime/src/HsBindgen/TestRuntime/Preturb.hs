module HsBindgen.TestRuntime.Preturb (
    -- * Preturb
    Preturb(..)
    -- * Properties
  , namePreturb0XSameSemanticsX
  , prop_Preturb0XSameSemanticsX
  , assertPreturb0XSameSemanticsX
  , nameNotPreturb1XSameSemanticsX
  , prop_NotPreturb1XSameSemanticsX
  , assertNotPreturb1XSameSemanticsX
  , namePreturbNegateNPreturbNXSameSemanticsX
  , prop_PreturbNegateNPreturbNXSameSemanticsX
  , assertPreturbNegateNPreturbNXSameSemanticsX
  , nameHsPreturbNXSameSemanticsCPreturbNX
  , prop_HsPreturbNXSameSemanticsCPreturbNX
  , assertHsPreturbNXSameSemanticsCPreturbNX
  ) where

import Control.Monad (unless)
import Data.Bits qualified as Bits
import Data.Word (Word32, Word64)
import Foreign.C.Types qualified as FC
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.QuickCheck (Property, discard)

import HsBindgen.TestRuntime.RealFloat qualified as RF
import HsBindgen.TestRuntime.SameSemantics (SameSemantics (sameSemantics),
                                            (/=~), (==~), (@/=~?), (@==~?))

{-------------------------------------------------------------------------------
  Preturb
-------------------------------------------------------------------------------}

-- | Preturb a value
class Preturb a where
  -- | Indexed perturbation
  --
  -- The @size@ is interpreted on a per-type basis.  Intuitively, smaller
  -- perturbations should be easier to understand (to support shrinking).
  --
  -- Type 'FC.CLLong' is used so that the Haskell implementation can match the C
  -- implementation.
  --
  -- prop> preturb 0 x `sameSemantics` x
  --
  -- prop> not (preturb 1 x `sameSemantics` x)
  --
  -- prop> preturb (negate size) (preturb size x) `sameSemantics` x
  preturb :: FC.CLLong -> a -> a

instance Preturb FC.CChar where
  preturb size n = n + fromIntegral size

instance Preturb FC.CSChar where
  preturb size n = n + fromIntegral size

instance Preturb FC.CUChar where
  preturb size n = n + fromIntegral size

instance Preturb FC.CShort where
  preturb size n = n + fromIntegral size

instance Preturb FC.CUShort where
  preturb size n = n + fromIntegral size

instance Preturb FC.CInt where
  preturb size n = n + fromIntegral size

instance Preturb FC.CUInt where
  preturb size n = n + fromIntegral size

instance Preturb FC.CLong where
  preturb size n = n + fromIntegral size

instance Preturb FC.CULong where
  preturb size n = n + fromIntegral size

instance Preturb FC.CPtrdiff where
  preturb size n = n + fromIntegral size

instance Preturb FC.CSize where
  preturb size n = n + fromIntegral size

instance Preturb FC.CWchar where
  preturb size n = n + fromIntegral size

instance Preturb FC.CSigAtomic where
  preturb size n = n + fromIntegral size

instance Preturb FC.CLLong where
  preturb size n = n + size

instance Preturb FC.CULLong where
  preturb size n = n + fromIntegral size

instance Preturb FC.CBool where
  preturb size (FC.CBool n) =
    FC.CBool $ if (size `mod` 2 == 1) /= (n > 0) then 1 else 0

instance Preturb FC.CIntPtr where
  preturb size n = n + fromIntegral size

instance Preturb FC.CUIntPtr where
  preturb size n = n + fromIntegral size

instance Preturb FC.CIntMax where
  preturb size n = n + fromIntegral size

instance Preturb FC.CUIntMax where
  preturb size n = n + fromIntegral size

instance Preturb FC.CClock where
  preturb size (FC.CClock n) = FC.CClock $ n + fromIntegral size

instance Preturb FC.CTime where
  preturb size (FC.CTime n) = FC.CTime $ n + fromIntegral size

instance Preturb FC.CFloat where
  preturb size (FC.CFloat x) = FC.CFloat $ preturbFloat size x

instance Preturb FC.CDouble where
  preturb size (FC.CDouble x) = FC.CDouble $ preturbDouble size x

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | A value preturbed with size 0 has the same semantics as the original value
namePreturb0XSameSemanticsX :: String
namePreturb0XSameSemanticsX = "Preturb0XSameSemanticsX"

-- | A value preturbed with size 0 has the same semantics as the original value
prop_Preturb0XSameSemanticsX ::
     (Preturb a, SameSemantics a, Show a)
  => a
  -> Property
prop_Preturb0XSameSemanticsX x = preturb 0 x ==~ x

-- | A value preturbed with size 0 has the same semantics as the original value
assertPreturb0XSameSemanticsX ::
     (Preturb a, SameSemantics a, Show a)
  => a
  -> Assertion
assertPreturb0XSameSemanticsX x = preturb 0 x @==~? x

-- | A value preturbed with size 1 does /not/ have the same semantics as the
-- original value
nameNotPreturb1XSameSemanticsX :: String
nameNotPreturb1XSameSemanticsX = "NotPreturb1XSameSemanticsX"

-- | A value preturbed with size 1 does /not/ have the same semantics as the
-- original value
prop_NotPreturb1XSameSemanticsX ::
     (Preturb a, SameSemantics a, Show a)
  => a
  -> Property
prop_NotPreturb1XSameSemanticsX x = preturb 1 x /=~ x

-- | A value preturbed with size 1 does /not/ have the same semantics as the
-- original value
assertNotPreturb1XSameSemanticsX ::
     (Preturb a, SameSemantics a, Show a)
  => a
  -> Assertion
assertNotPreturb1XSameSemanticsX x = preturb 1 x @/=~? x

-- | A value preturbed with a size and the negated size has the same semantics
-- as the original value
--
-- In other words, @'preturb' (negate size)@ is the inverse of @'preturb' size@.
--
-- This property does not apply when the size is @0@ or @minBound@.
namePreturbNegateNPreturbNXSameSemanticsX :: String
namePreturbNegateNPreturbNXSameSemanticsX =
    "PreturbNegateNPreturbNXSameSemanticsX"

-- | A value preturbed with a size and the negated size has the same semantics
-- as the original value
--
-- In other words, @'preturb' (negate size)@ is the inverse of @'preturb' size@.
--
-- This property does not apply when the size is @0@ or @minBound@.
prop_PreturbNegateNPreturbNXSameSemanticsX ::
     (Preturb a, SameSemantics a, Show a)
  => FC.CLLong
  -> a
  -> Property
prop_PreturbNegateNPreturbNXSameSemanticsX size x
  | size == 0        = discard
  | size == minBound = discard
  | otherwise        = preturb (negate size) (preturb size x) ==~ x

-- | A value preturbed with a size and the negated size has the same semantics
-- as the original value
--
-- In other words, @'preturb' (negate size)@ is the inverse of @'preturb' size@.
--
-- This property does not apply when the size is @minBound@.
assertPreturbNegateNPreturbNXSameSemanticsX ::
     (Preturb a, SameSemantics a, Show a)
  => FC.CLLong
  -> a
  -> Assertion
assertPreturbNegateNPreturbNXSameSemanticsX size x =
    unless (size == 0 || size == minBound) $
      preturb (negate size) (preturb size x) @==~? x

-- | A value preturbed using Haskell has the same semantics as the value
-- preturbed using C (using the same size)
nameHsPreturbNXSameSemanticsCPreturbNX :: String
nameHsPreturbNXSameSemanticsCPreturbNX = "HsPreturbNXSameSemanticsCPreturbNX"

-- | A value preturbed using Haskell has the same semantics as the value
-- preturbed using C (using the same size)
prop_HsPreturbNXSameSemanticsCPreturbNX ::
     (Preturb a, SameSemantics a)
  => (FC.CLLong -> a -> IO a)
  -> FC.CLLong
  -> a
  -> Property
prop_HsPreturbNXSameSemanticsCPreturbNX cPreturb size x = QCM.monadicIO $ do
    x' <- QCM.run $ cPreturb size x
    QCM.assert $ preturb size x `sameSemantics` x'

-- | A value preturbed using Haskell has the same semantics as the value
-- preturbed using C (using the same size)
assertHsPreturbNXSameSemanticsCPreturbNX ::
     (Preturb a, SameSemantics a, Show a)
  => (FC.CLLong -> a -> IO a)
  -> FC.CLLong
  -> a
  -> Assertion
assertHsPreturbNXSameSemanticsCPreturbNX cPreturb size x = do
    x' <- cPreturb size x
    preturb size x @==~? x'

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Preturb a 'Float' value
--
-- Different kinds of values are preturbed separately:
--
-- * NaN and negative zero are preturbed to each other.
-- * Positive and negative infinity are preturbed to each other.
-- * Zero and subnormal values are preturbed as follows.
--     1. Fraction
--     2. Sign
-- * Normal values are preturbed as follows.
--     1. Fraction
--     2. Exponent
--     3. Sign
preturbFloat :: FC.CLLong -> Float -> Float
-- Any changes to this implementation must also be made in the C implementation.
-- Note that the @'@ character is not used in variable names so that variable
-- names can be translated to C, making it easier to compare the two
-- implementations.
preturbFloat size x
    | isNaN x          = if even size then RF.nan     else RF.negZero
    | isNegativeZero x = if even size then RF.negZero else RF.nan
    | isInfinite x     = if even size then x          else negate x
    | e == 0 = RF.floatFromWord32 $
        let sR = if even (sizeDiv + fDiv) then s else sC
        in  sR + fromIntegral fMod
    | otherwise = RF.floatFromWord32 $
        let (eDiv, eMod) = fmap (+ 1) $
              (fDiv + sizeDiv + fromIntegral e - 1) `divMod` eSize
            sR = if even eDiv then s else sC
        in  sR + Bits.shiftL (fromIntegral eMod) eIdx + fromIntegral fMod
  where
    -- Single-precision floating-point values are represented as follows:
    --
    -- 3  2          1         0
    -- 1 09876543 21098765432109876543210
    -- s ---e8--- ----------f23----------
    --
    -- There are three groups:
    --
    -- * 1 sign bit (0: negative, 1: positive)
    -- * 8 exponent bits; special cases:
    --     * All zero bits:
    --         * Positive or negative zero when fraction is zero
    --         * Subnormal number when fraction is non-zero
    --     * All one bits
    --         * Positive or negative infinity when fraction is zero
    --         * NaN when fraction is non-zero
    --     * All one bits: infinite of NaN
    -- * 23 fraction (significand) bits
    --
    -- <https://en.wikipedia.org/wiki/Single-precision_floating-point_format>

    -- Sign bitmask (not shifted), eponent bitmask (shifted), fraction bitmask
    sMask, eMask, fMask :: Word32
    sMask = 0x80000000
    eMask = 0xff
    fMask = 0x7fffff

    -- Exponent index (least significant bit)
    eIdx :: Int
    eIdx = 23

    -- Exponent size (not including special values), fraction size
    eSize, fSize :: FC.CLLong
    eSize = fromIntegral $ eMask - 1
    fSize = fromIntegral $ fMask + 1

    -- w: passed value as a word to preturb at the representation level
    -- s: masked sign bit (not shifted)
    -- sC: complemented masked sign bit (not shifted)
    -- e: exponent (shifted)
    -- f: fraction
    w, s, sC, e, f :: Word32
    w  = RF.floatToWord32 x
    s  = w Bits..&. sMask
    sC = Bits.xor s sMask
    e  = Bits.shiftR w eIdx Bits..&. eMask
    f  = w Bits..&. fMask

    -- Avoid overflow in CLLong domain
    sizeDiv, sizeMod, fDiv, fMod :: FC.CLLong
    (sizeDiv, sizeMod) = size `divMod` fSize
    (fDiv, fMod) = (sizeMod + fromIntegral f) `divMod` fSize

-- | Preturb a 'Double' value
--
-- Different kinds of values are preturbed separately:
--
-- * NaN and negative zero are preturbed to each other.
-- * Positive and negative infinity are preturbed to each other.
-- * Zero and subnormal values are preturbed as follows.
--     1. Fraction
--     2. Sign
-- * Normal values are preturbed as follows.
--     1. Fraction
--     2. Exponent
--     3. Sign
preturbDouble :: FC.CLLong -> Double -> Double
-- Any changes to this implementation must also be made in the C implementation.
-- Note that the @'@ character is not used in variable names so that variable
-- names can be translated to C, making it easier to compare the two
-- implementations.
preturbDouble size x
    | isNaN x          = if even size then RF.nan     else RF.negZero
    | isNegativeZero x = if even size then RF.negZero else RF.nan
    | isInfinite x     = if even size then x          else negate x
    | e == 0 = RF.doubleFromWord64 $
        let sR = if even (sizeDiv + fDiv) then s else sC
        in  sR + fromIntegral fMod
    | otherwise = RF.doubleFromWord64 $
        let (eDiv, eMod) = fmap (+ 1) $
              (fDiv + sizeDiv + fromIntegral e - 1) `divMod` eSize
            sR = if even eDiv then s else sC
        in  sR + Bits.shiftL (fromIntegral eMod) eIdx + fromIntegral fMod
  where
    -- Double-precision floating-point values are represented as follows:
    --
    -- 6    5          4         3         2         1         0
    -- 3 21098765432 1098765432109876543210987654321098765432109876543210
    -- s ----e11---- ------------------------f52-------------------------
    --
    -- There are three groups:
    --
    -- * 1 sign bit (0: negative, 1: positive)
    -- * 11 exponent bits; special cases:
    --     * All zero bits:
    --         * Positive or negative zero when fraction is zero
    --         * Subnormal number when fraction is non-zero
    --     * All one bits
    --         * Positive or negative infinity when fraction is zero
    --         * NaN when fraction is non-zero
    --     * All one bits: infinite of NaN
    -- * 52 fraction (significand) bits
    --
    -- <https://en.wikipedia.org/wiki/Double-precision_floating-point_format>

    -- Sign bitmask (not shifted), eponent bitmask (shifted), fraction bitmask
    sMask, eMask, fMask :: Word64
    sMask = 0x8000000000000000
    eMask = 0x7ff
    fMask = 0xfffffffffffff

    -- Exponent index (least significant bit)
    eIdx :: Int
    eIdx = 52

    -- Exponent size (not including special values), fraction size
    eSize, fSize :: FC.CLLong
    eSize = fromIntegral $ eMask - 1
    fSize = fromIntegral $ fMask + 1

    -- w: passed value as a word to preturb at the representation level
    -- s: masked sign bit (not shifted)
    -- sC: complemented masked sign bit (not shifted)
    -- e: exponent (shifted)
    -- f: fraction
    w, s, sC, e, f :: Word64
    w  = RF.doubleToWord64 x
    s  = w Bits..&. sMask
    sC = Bits.xor s sMask
    e  = Bits.shiftR w eIdx Bits..&. eMask
    f  = w Bits..&. fMask

    -- Avoid overflow in CLLong domain
    sizeDiv, sizeMod, fDiv, fMod :: FC.CLLong
    (sizeDiv, sizeMod) = size `divMod` fSize
    (fDiv, fMod) = (sizeMod + fromIntegral f) `divMod` fSize
