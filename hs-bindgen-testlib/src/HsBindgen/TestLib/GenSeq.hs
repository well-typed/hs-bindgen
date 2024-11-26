module HsBindgen.TestLib.GenSeq (
    -- * GenSeq
    GenSeq (..)
    -- * Properties
  , nameHsGenSeqNSameSemanticsCGenSeqN
  , prop_HsGenSeqNSameSemanticsCGenSeqN
  , assertHsGenSeqNSameSemanticsCGenSeqN
  , nameHsGenSeq1CEq
  , assertHsGenSeq1CEq
  , nameHsGenSeq1SameSemanticsCGenSeq1
  , assertHsGenSeq1SameSemanticsCGenSeq1
  ) where

import Control.Monad (unless)
import Foreign.C qualified as FC
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty.HUnit (Assertion, assertFailure)
import Test.Tasty.QuickCheck (Property)

import HsBindgen.TestLib.SameSemantics ((@==~?), SameSemantics(sameSemantics))

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | Ordinal value of the letter @A@
ordA :: FC.CULong
ordA = 65

{-------------------------------------------------------------------------------
  GenSeq
-------------------------------------------------------------------------------}

class GenSeq a where
  genSeq :: FC.CULong -> a

instance GenSeq FC.CChar where
  genSeq = fromIntegral . (+ ordA)

instance GenSeq FC.CSChar where
  genSeq = fromIntegral . (+ ordA)

instance GenSeq FC.CUChar where
  genSeq = fromIntegral . (+ ordA)

instance GenSeq FC.CShort where
  genSeq = fromIntegral

instance GenSeq FC.CUShort where
  genSeq = fromIntegral

instance GenSeq FC.CInt where
  genSeq = fromIntegral

instance GenSeq FC.CUInt where
  genSeq = fromIntegral

instance GenSeq FC.CLong where
  genSeq = fromIntegral

instance GenSeq FC.CULong where
  genSeq = id

instance GenSeq FC.CPtrdiff where
  genSeq = fromIntegral

instance GenSeq FC.CSize where
  genSeq = fromIntegral

instance GenSeq FC.CWchar where
  genSeq = fromIntegral . (+ ordA)

instance GenSeq FC.CSigAtomic where
  genSeq = fromIntegral

instance GenSeq FC.CLLong where
  genSeq = fromIntegral

instance GenSeq FC.CULLong where
  genSeq = fromIntegral

instance GenSeq FC.CBool where
  genSeq = fromIntegral . (`mod` 2)

instance GenSeq FC.CIntPtr where
  genSeq = fromIntegral

instance GenSeq FC.CUIntPtr where
  genSeq = fromIntegral

instance GenSeq FC.CIntMax where
  genSeq = fromIntegral

instance GenSeq FC.CUIntMax where
  genSeq = fromIntegral

instance GenSeq FC.CClock where
  genSeq = fromIntegral

instance GenSeq FC.CTime where
  genSeq = fromIntegral

instance GenSeq FC.CFloat where
  genSeq = fromIntegral

instance GenSeq FC.CDouble where
  genSeq = fromIntegral

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | Values generated from the same sequential value in Haskell and C have the
-- same semantics
nameHsGenSeqNSameSemanticsCGenSeqN :: String
nameHsGenSeqNSameSemanticsCGenSeqN = "HsGenSeqNSameSemanticsCGenSeqN"

-- | Values generated from the same sequential value in Haskell and C have the
-- same semantics
prop_HsGenSeqNSameSemanticsCGenSeqN ::
     (GenSeq a, SameSemantics a)
  => (FC.CULong -> IO a)
  -> FC.CULong
  -> Property
prop_HsGenSeqNSameSemanticsCGenSeqN cGenSeq n = QCM.monadicIO $ do
    x <- QCM.run $ cGenSeq n
    QCM.assert $ genSeq n `sameSemantics` x

-- | Values generated from the same sequential value in Haskell and C have the
-- same semantics
assertHsGenSeqNSameSemanticsCGenSeqN ::
     (GenSeq a, SameSemantics a, Show a)
  => (FC.CULong -> IO a)
  -> FC.CULong
  -> Assertion
assertHsGenSeqNSameSemanticsCGenSeqN cGenSeq n = do
    x <- cGenSeq n
    genSeq n @==~? x

-- | A value sequentially generated in Haskell has the expected value in C
nameHsGenSeq1CEq :: String
nameHsGenSeq1CEq = "HsGenSeq1CEq"

-- | A value sequentially generated in Haskell has the expected value in C
assertHsGenSeq1CEq :: (GenSeq a, Show a) => (a -> IO Bool) -> Assertion
assertHsGenSeq1CEq cAssert = do
    let x = genSeq 1
    isSuccess <- cAssert x
    unless isSuccess . assertFailure $ show x

-- | A value sequentially generated in C has the expected value in Haskell
nameHsGenSeq1SameSemanticsCGenSeq1 :: String
nameHsGenSeq1SameSemanticsCGenSeq1 = "HsGenSeq1SameSemanticsCGenSeq1"

-- | A value sequentially generated in C has the expected value in Haskell
assertHsGenSeq1SameSemanticsCGenSeq1 ::
     (GenSeq a, SameSemantics a, Show a)
  => IO a
  -> Assertion
assertHsGenSeq1SameSemanticsCGenSeq1 cSeqGen1 = do
    x <- cSeqGen1
    genSeq 1 @==~? x
