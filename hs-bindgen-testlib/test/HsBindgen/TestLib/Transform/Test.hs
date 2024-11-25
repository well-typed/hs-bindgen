module HsBindgen.TestLib.Transform.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestLib.Arbitrary ()
import HsBindgen.TestLib.CLib qualified as CLib
import HsBindgen.TestLib.RealFloat qualified as RF
import HsBindgen.TestLib.SameSemantics (SameSemantics)
import HsBindgen.TestLib.Transform
  ( Transform, assertTransformHsSameSemanticsC
  , assertTransformVNotSameSemanticsV, prop_TransformVNotSameSemanticsV
  , prop_TransformHsSameSemanticsC
  )

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a 'Num'
testNum :: forall a.
     (Arbitrary a, Num a, SameSemantics a, Show a, Transform a, Typeable a)
  => (a -> IO a)
  -> TestTree
testNum cTransform = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "TransformVNotSameSemanticsV"
        [ testCase "zero" $ assertTransformVNotSameSemanticsV @a 0
        , testProperty "random" $ prop_TransformVNotSameSemanticsV @a
        ]
    , testGroup "TransformHsSameSemanticsC"
        [ testCase "zero" $ assertTransformHsSameSemanticsC cTransform 0
        , testProperty "random" $ prop_TransformHsSameSemanticsC cTransform
        ]
    ]

-- | Test a 'Bounded' 'Num'
testBoundedNum :: forall a.
     ( Arbitrary a
     , Bounded a
     , Num a
     , SameSemantics a
     , Show a
     , Transform a
     , Typeable a
     )
  => (a -> IO a)
  -> TestTree
testBoundedNum cTransform = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "TransformVNotSameSemanticsV"
        [ testCase "zero" $ assertTransformVNotSameSemanticsV @a 0
        , testCase "min"  $ assertTransformVNotSameSemanticsV @a minBound
        , testCase "max"  $ assertTransformVNotSameSemanticsV @a maxBound
        , testProperty "random" $ prop_TransformVNotSameSemanticsV @a
        ]
    , testGroup "TransformHsSameSemanticsC"
        [ testCase "zero" $ assertTransformHsSameSemanticsC cTransform 0
        , testCase "min"  $ assertTransformHsSameSemanticsC cTransform minBound
        , testCase "max"  $ assertTransformHsSameSemanticsC cTransform maxBound
        , testProperty "random" $ prop_TransformHsSameSemanticsC cTransform
        ]
    ]

-- | Test a 'RealFloat'
testRealFloat :: forall a.
     ( Arbitrary a
     , RealFloat a
     , SameSemantics a
     , Show a
     , Transform a
     , Typeable a
     )
  => (a -> IO a)
  -> TestTree
testRealFloat cTransform = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "TransformVNotSameSemanticsV"
        [ testCase "zero"  $ assertTransformVNotSameSemanticsV @a RF.zero
        , testCase "-zero" $ assertTransformVNotSameSemanticsV @a RF.negZero
        , testCase "min"   $ assertTransformVNotSameSemanticsV @a RF.minValue
        , testCase "max"   $ assertTransformVNotSameSemanticsV @a RF.maxValue
        , testCase "inf"   $ assertTransformVNotSameSemanticsV @a RF.inf
        , testCase "-inf"  $ assertTransformVNotSameSemanticsV @a RF.negInf
        , testCase "NaN"   $ assertTransformVNotSameSemanticsV @a RF.nan
        , testProperty "random" $ prop_TransformVNotSameSemanticsV @a
        ]
    , testGroup "TransformHsSameSemanticsC"
        [ testCase "zero" $ assertTransformHsSameSemanticsC cTransform RF.zero
        , testCase "-zero" $
            assertTransformHsSameSemanticsC cTransform RF.negZero
        , testCase "min" $
            assertTransformHsSameSemanticsC cTransform RF.minValue
        , testCase "max" $
            assertTransformHsSameSemanticsC cTransform RF.maxValue
        , testCase "inf" $ assertTransformHsSameSemanticsC cTransform RF.inf
        , testCase "-inf" $ assertTransformHsSameSemanticsC cTransform RF.negInf
        , testCase "NaN" $ assertTransformHsSameSemanticsC cTransform RF.nan
        , testProperty "random" $ prop_TransformHsSameSemanticsC cTransform
        ]
    ]

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestLib.Transform"
    [ testBoundedNum CLib.transformCChar
    , testBoundedNum CLib.transformCSChar
    , testBoundedNum CLib.transformCUChar
    , testBoundedNum CLib.transformCShort
    , testBoundedNum CLib.transformCUShort
    , testBoundedNum CLib.transformCInt
    , testBoundedNum CLib.transformCUInt
    , testBoundedNum CLib.transformCLong
    , testBoundedNum CLib.transformCULong
    , testBoundedNum CLib.transformCPtrdiff
    , testBoundedNum CLib.transformCSize
    , testBoundedNum CLib.transformCWchar
    , testBoundedNum CLib.transformCSigAtomic
    , testBoundedNum CLib.transformCLLong
    , testBoundedNum CLib.transformCULLong
    , testBoundedNum CLib.transformCBool
    , testBoundedNum CLib.transformCIntPtr
    , testBoundedNum CLib.transformCUIntPtr
    , testBoundedNum CLib.transformCIntMax
    , testBoundedNum CLib.transformCUIntMax
    , testNum        CLib.transformCClock
    , testNum        CLib.transformCTime
    , testRealFloat  CLib.transformCFloat
    , testRealFloat  CLib.transformCDouble
    ]
