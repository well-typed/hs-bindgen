module HsBindgen.TestRuntime.Perturb (
    -- * Perturb
    Perturb(..)
    -- * Properties
  , namePerturb0XSameSemanticsX
  , prop_Perturb0XSameSemanticsX
  , assertPerturb0XSameSemanticsX
  , nameNotPerturb1XSameSemanticsX
  , prop_NotPerturb1XSameSemanticsX
  , assertNotPerturb1XSameSemanticsX
  , namePerturbNegateNPerturbNXSameSemanticsX
  , prop_PerturbNegateNPerturbNXSameSemanticsX
  , assertPerturbNegateNPerturbNXSameSemanticsX
  , nameHsPerturbNXSameSemanticsCPerturbNX
  , prop_HsPerturbNXSameSemanticsCPerturbNX
  , assertHsPerturbNXSameSemanticsCPerturbNX
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
  Perturb
-------------------------------------------------------------------------------}

-- | Perturb a value
class Perturb a where
  -- | Indexed perturbation
  --
  -- The @size@ is interpreted on a per-type basis.  Intuitively, smaller
  -- perturbations should be easier to understand (to support shrinking).
  --
  -- Type 'FC.CLLong' is used so that the Haskell implementation can match the C
  -- implementation.
  --
  -- prop> perturb 0 x `sameSemantics` x
  --
  -- prop> not (perturb 1 x `sameSemantics` x)
  --
  -- prop> perturb (negate size) (perturb size x) `sameSemantics` x
  perturb :: FC.CLLong -> a -> a

instance Perturb FC.CChar where
  perturb size n = n + fromIntegral size

instance Perturb FC.CSChar where
  perturb size n = n + fromIntegral size

instance Perturb FC.CUChar where
  perturb size n = n + fromIntegral size

instance Perturb FC.CShort where
  perturb size n = n + fromIntegral size

instance Perturb FC.CUShort where
  perturb size n = n + fromIntegral size

instance Perturb FC.CInt where
  perturb size n = n + fromIntegral size

instance Perturb FC.CUInt where
  perturb size n = n + fromIntegral size

instance Perturb FC.CLong where
  perturb size n = n + fromIntegral size

instance Perturb FC.CULong where
  perturb size n = n + fromIntegral size

instance Perturb FC.CPtrdiff where
  perturb size n = n + fromIntegral size

instance Perturb FC.CSize where
  perturb size n = n + fromIntegral size

instance Perturb FC.CWchar where
  perturb size n = n + fromIntegral size

instance Perturb FC.CSigAtomic where
  perturb size n = n + fromIntegral size

instance Perturb FC.CLLong where
  perturb size n = n + size

instance Perturb FC.CULLong where
  perturb size n = n + fromIntegral size

instance Perturb FC.CBool where
  perturb size (FC.CBool n) =
    FC.CBool $ if (size `mod` 2 == 1) /= (n > 0) then 1 else 0

instance Perturb FC.CIntPtr where
  perturb size n = n + fromIntegral size

instance Perturb FC.CUIntPtr where
  perturb size n = n + fromIntegral size

instance Perturb FC.CIntMax where
  perturb size n = n + fromIntegral size

instance Perturb FC.CUIntMax where
  perturb size n = n + fromIntegral size

instance Perturb FC.CClock where
  perturb size (FC.CClock n) = FC.CClock $ n + fromIntegral size

instance Perturb FC.CTime where
  perturb size (FC.CTime n) = FC.CTime $ n + fromIntegral size

instance Perturb FC.CFloat where
  perturb size (FC.CFloat x) = FC.CFloat $ perturbFloat size x

instance Perturb FC.CDouble where
  perturb size (FC.CDouble x) = FC.CDouble $ perturbDouble size x

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | A value perturbed with size 0 has the same semantics as the original value
namePerturb0XSameSemanticsX :: String
namePerturb0XSameSemanticsX = "Perturb0XSameSemanticsX"

-- | A value perturbed with size 0 has the same semantics as the original value
prop_Perturb0XSameSemanticsX ::
     (Perturb a, SameSemantics a, Show a)
  => a
  -> Property
prop_Perturb0XSameSemanticsX x = perturb 0 x ==~ x

-- | A value perturbed with size 0 has the same semantics as the original value
assertPerturb0XSameSemanticsX ::
     (Perturb a, SameSemantics a, Show a)
  => a
  -> Assertion
assertPerturb0XSameSemanticsX x = perturb 0 x @==~? x

-- | A value perturbed with size 1 does /not/ have the same semantics as the
-- original value
nameNotPerturb1XSameSemanticsX :: String
nameNotPerturb1XSameSemanticsX = "NotPerturb1XSameSemanticsX"

-- | A value perturbed with size 1 does /not/ have the same semantics as the
-- original value
prop_NotPerturb1XSameSemanticsX ::
     (Perturb a, SameSemantics a, Show a)
  => a
  -> Property
prop_NotPerturb1XSameSemanticsX x = perturb 1 x /=~ x

-- | A value perturbed with size 1 does /not/ have the same semantics as the
-- original value
assertNotPerturb1XSameSemanticsX ::
     (Perturb a, SameSemantics a, Show a)
  => a
  -> Assertion
assertNotPerturb1XSameSemanticsX x = perturb 1 x @/=~? x

-- | A value perturbed with a size and the negated size has the same semantics
-- as the original value
--
-- In other words, @'perturb' (negate size)@ is the inverse of @'perturb' size@.
--
-- This property does not apply when the size is @0@ or @minBound@.
namePerturbNegateNPerturbNXSameSemanticsX :: String
namePerturbNegateNPerturbNXSameSemanticsX =
    "PerturbNegateNPerturbNXSameSemanticsX"

-- | A value perturbed with a size and the negated size has the same semantics
-- as the original value
--
-- In other words, @'perturb' (negate size)@ is the inverse of @'perturb' size@.
--
-- This property does not apply when the size is @0@ or @minBound@.
prop_PerturbNegateNPerturbNXSameSemanticsX ::
     (Perturb a, SameSemantics a, Show a)
  => FC.CLLong
  -> a
  -> Property
prop_PerturbNegateNPerturbNXSameSemanticsX size x
  | size == 0        = discard
  | size == minBound = discard
  | otherwise        = perturb (negate size) (perturb size x) ==~ x

-- | A value perturbed with a size and the negated size has the same semantics
-- as the original value
--
-- In other words, @'perturb' (negate size)@ is the inverse of @'perturb' size@.
--
-- This property does not apply when the size is @minBound@.
assertPerturbNegateNPerturbNXSameSemanticsX ::
     (Perturb a, SameSemantics a, Show a)
  => FC.CLLong
  -> a
  -> Assertion
assertPerturbNegateNPerturbNXSameSemanticsX size x =
    unless (size == 0 || size == minBound) $
      perturb (negate size) (perturb size x) @==~? x

-- | A value perturbed using Haskell has the same semantics as the value
-- perturbed using C (using the same size)
nameHsPerturbNXSameSemanticsCPerturbNX :: String
nameHsPerturbNXSameSemanticsCPerturbNX = "HsPerturbNXSameSemanticsCPerturbNX"

-- | A value perturbed using Haskell has the same semantics as the value
-- perturbed using C (using the same size)
prop_HsPerturbNXSameSemanticsCPerturbNX ::
     (Perturb a, SameSemantics a)
  => (FC.CLLong -> a -> IO a)
  -> FC.CLLong
  -> a
  -> Property
prop_HsPerturbNXSameSemanticsCPerturbNX cPerturb size x = QCM.monadicIO $ do
    x' <- QCM.run $ cPerturb size x
    QCM.assert $ perturb size x `sameSemantics` x'

-- | A value perturbed using Haskell has the same semantics as the value
-- perturbed using C (using the same size)
assertHsPerturbNXSameSemanticsCPerturbNX ::
     (Perturb a, SameSemantics a, Show a)
  => (FC.CLLong -> a -> IO a)
  -> FC.CLLong
  -> a
  -> Assertion
assertHsPerturbNXSameSemanticsCPerturbNX cPerturb size x = do
    x' <- cPerturb size x
    perturb size x @==~? x'

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Perturb a 'Float' value
--
-- Different kinds of values are perturbed separately:
--
-- * NaN and negative zero are perturbed to each other.
-- * Positive and negative infinity are perturbed to each other.
-- * Zero and subnormal values are perturbed as follows.
--     1. Fraction
--     2. Sign
-- * Normal values are perturbed as follows.
--     1. Fraction
--     2. Exponent
--     3. Sign
perturbFloat :: FC.CLLong -> Float -> Float
-- Any changes to this implementation must also be made in the C implementation.
-- Note that the @'@ character is not used in variable names so that variable
-- names can be translated to C, making it easier to compare the two
-- implementations.
perturbFloat size x
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

    -- w: passed value as a word to perturb at the representation level
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

-- | Perturb a 'Double' value
--
-- Different kinds of values are perturbed separately:
--
-- * NaN and negative zero are perturbed to each other.
-- * Positive and negative infinity are perturbed to each other.
-- * Zero and subnormal values are perturbed as follows.
--     1. Fraction
--     2. Sign
-- * Normal values are perturbed as follows.
--     1. Fraction
--     2. Exponent
--     3. Sign
perturbDouble :: FC.CLLong -> Double -> Double
-- Any changes to this implementation must also be made in the C implementation.
-- Note that the @'@ character is not used in variable names so that variable
-- names can be translated to C, making it easier to compare the two
-- implementations.
perturbDouble size x
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

    -- w: passed value as a word to perturb at the representation level
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
