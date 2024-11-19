module HsBindgen.TestLib.Transform.Test (tests) where

import Foreign.C qualified as FC
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)
import Test.Tasty.QuickCheck (Property, testProperty)

import HsBindgen.TestLib.CLib qualified as CLib
import HsBindgen.TestLib.RealFloat qualified as RF
import HsBindgen.TestLib.RepEq ((@!=?), RepEq(repEq))
import HsBindgen.TestLib.Transform (Transform (transform))

{-------------------------------------------------------------------------------
  Auxilliary functions
-------------------------------------------------------------------------------}

-- | Results of C and Haskell transformations of the specified value are
-- representationally equal
prop_HsEqC :: (RepEq a, Transform a) => (a -> IO a) -> a -> Property
prop_HsEqC cTransform x = QCM.monadicIO $ do
    x' <- QCM.run $ cTransform x
    QCM.assert $ transform x `repEq` x'

-- | Assert that the results of C and Haskell transformations of the specified
-- value are representationally equal
assertXRepEq :: (RepEq a, Show a, Transform a) => (a -> IO a) -> a -> Assertion
assertXRepEq cTransform x = do
    x' <- cTransform x
    transform x @!=? x'

{-------------------------------------------------------------------------------
  Type tests
-------------------------------------------------------------------------------}

testCChar :: TestTree
testCChar = testGroup "CChar"
    [ testCase "zero"      $ assertXRepEq cX 0
    , testCase "min"       $ assertXRepEq cX minBound
    , testCase "max"       $ assertXRepEq cX maxBound
    , testProperty "HsEqC" $ prop_HsEqC cX
    ]
  where
    cX :: FC.CChar -> IO FC.CChar
    cX = CLib.transformCChar

testCInt :: TestTree
testCInt = testGroup "CInt"
    [ testCase "zero"      $ assertXRepEq cX 0
    , testCase "min"       $ assertXRepEq cX minBound
    , testCase "max"       $ assertXRepEq cX maxBound
    , testProperty "HsEqC" $ prop_HsEqC cX
    ]
  where
    cX :: FC.CInt -> IO FC.CInt
    cX = CLib.transformCInt

testCFloat :: TestTree
testCFloat = testGroup "CFloat"
    [ testCase "zero"      $ assertXRepEq cX RF.zero
    , testCase "min"       $ assertXRepEq cX RF.minValue
    , testCase "max"       $ assertXRepEq cX RF.maxValue
    , testCase "-zero"     $ assertXRepEq cX RF.negZero
    , testCase "infinity"  $ assertXRepEq cX RF.inf
    , testCase "-infinity" $ assertXRepEq cX RF.negInf
    , testCase "NaN"       $ assertXRepEq cX RF.nan
    , testProperty "HsEqC" $ prop_HsEqC cX
    ]
  where
    cX :: FC.CFloat -> IO FC.CFloat
    cX = CLib.transformCFloat

testCDouble :: TestTree
testCDouble = testGroup "CDouble"
    [ testCase "zero"      $ assertXRepEq cX RF.zero
    , testCase "min"       $ assertXRepEq cX RF.minValue
    , testCase "max"       $ assertXRepEq cX RF.maxValue
    , testCase "-zero"     $ assertXRepEq cX RF.negZero
    , testCase "infinity"  $ assertXRepEq cX RF.inf
    , testCase "-infinity" $ assertXRepEq cX RF.negInf
    , testCase "NaN"       $ assertXRepEq cX RF.nan
    , testProperty "HsEqC" $ prop_HsEqC cX
    ]
  where
    cX :: FC.CDouble -> IO FC.CDouble
    cX = CLib.transformCDouble

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestLib.Transform"
    [ testCChar
    , testCInt
    , testCFloat
    , testCDouble
    ]
