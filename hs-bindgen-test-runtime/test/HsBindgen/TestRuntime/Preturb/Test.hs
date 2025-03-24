module HsBindgen.TestRuntime.Preturb.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestRuntime.Arbitrary ()
import HsBindgen.TestRuntime.CLib qualified as CLib
import HsBindgen.TestRuntime.RealFloat qualified as RF
import HsBindgen.TestRuntime.SameSemantics (SameSemantics)
import HsBindgen.TestRuntime.Preturb (Preturb)
import HsBindgen.TestRuntime.Preturb qualified as Preturb

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a 'Num'
testNum :: forall a.
     (Arbitrary a, Num a, Preturb a, SameSemantics a, Show a, Typeable a)
  => (FC.CLLong -> a -> IO a)
  -> TestTree
testNum cX = testGroup (show (typeRep (Proxy @a)))
    [ testGroup Preturb.namePreturb0XSameSemanticsX
        [ testCase "zero" $ Preturb.assertPreturb0XSameSemanticsX @a 0
        , testProperty "random" $ Preturb.prop_Preturb0XSameSemanticsX @a
        ]
    , testGroup Preturb.nameNotPreturb1XSameSemanticsX
        [ testCase "zero" $ Preturb.assertNotPreturb1XSameSemanticsX @a 0
        , testProperty "random" $ Preturb.prop_NotPreturb1XSameSemanticsX @a
        ]
    , testGroup Preturb.namePreturbNegateNPreturbNXSameSemanticsX
        [ testProperty "random" $
            Preturb.prop_PreturbNegateNPreturbNXSameSemanticsX @a
        ]
    , testGroup Preturb.nameHsPreturbNXSameSemanticsCPreturbNX
        [ testCase "size:zero" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 0 x
        , testProperty "random" $
            Preturb.prop_HsPreturbNXSameSemanticsCPreturbNX cX
        ]
    ]
  where
    x :: a
    x = 11

-- | Test a 'Bounded' 'Num'
testBoundedNum :: forall a.
     ( Arbitrary a
     , Bounded a
     , Num a
     , Preturb a
     , SameSemantics a
     , Show a
     , Typeable a
     )
  => (FC.CLLong -> a -> IO a)
  -> TestTree
testBoundedNum cX = testGroup (show (typeRep (Proxy @a)))
    [ testGroup Preturb.namePreturb0XSameSemanticsX
        [ testCase "zero" $ Preturb.assertPreturb0XSameSemanticsX @a 0
        , testCase "min" $ Preturb.assertPreturb0XSameSemanticsX @a minBound
        , testCase "max" $ Preturb.assertPreturb0XSameSemanticsX @a maxBound
        , testProperty "random" $ Preturb.prop_Preturb0XSameSemanticsX @a
        ]
    , testGroup Preturb.nameNotPreturb1XSameSemanticsX
        [ testCase "zero" $ Preturb.assertNotPreturb1XSameSemanticsX @a 0
        , testCase "min" $ Preturb.assertNotPreturb1XSameSemanticsX @a minBound
        , testCase "max" $ Preturb.assertNotPreturb1XSameSemanticsX @a maxBound
        , testProperty "random" $ Preturb.prop_NotPreturb1XSameSemanticsX @a
        ]
    , testGroup Preturb.namePreturbNegateNPreturbNXSameSemanticsX
        [ testCase "size:max" $
            Preturb.assertPreturbNegateNPreturbNXSameSemanticsX @a maxBound x
        , testProperty "random" $
            Preturb.prop_PreturbNegateNPreturbNXSameSemanticsX @a
        ]
    , testGroup Preturb.nameHsPreturbNXSameSemanticsCPreturbNX
        [ testCase "zero" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 0
        , testCase "min" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 minBound
        , testCase "max" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 maxBound
        , testCase "size:zero" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 0 x
        , testCase "size:min" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX minBound x
        , testCase "size:max" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX maxBound x
        , testProperty "random" $
            Preturb.prop_HsPreturbNXSameSemanticsCPreturbNX cX
        ]
    ]
  where
    x :: a
    x = 11

-- | Test a 'RealFloat'
testRealFloat :: forall a.
     ( Arbitrary a
     , Preturb a
     , RealFloat a
     , SameSemantics a
     , Show a
     , Typeable a
     )
  => (FC.CLLong -> a -> IO a)
  -> TestTree
testRealFloat cX = testGroup (show (typeRep (Proxy @a)))
    [ testGroup Preturb.namePreturb0XSameSemanticsX
        [ testCase "zero" $ Preturb.assertPreturb0XSameSemanticsX @a RF.zero
        , testCase "-zero" $ Preturb.assertPreturb0XSameSemanticsX @a RF.negZero
        , testCase "min" $ Preturb.assertPreturb0XSameSemanticsX @a RF.minValue
        , testCase "max" $ Preturb.assertPreturb0XSameSemanticsX @a RF.maxValue
        , testCase "inf" $ Preturb.assertPreturb0XSameSemanticsX @a RF.inf
        , testCase "-inf" $ Preturb.assertPreturb0XSameSemanticsX @a RF.negInf
        , testCase "NaN" $ Preturb.assertPreturb0XSameSemanticsX @a RF.nan
        , testProperty "random" $ Preturb.prop_Preturb0XSameSemanticsX @a
        ]
    , testGroup Preturb.nameNotPreturb1XSameSemanticsX
        [ testCase "zero" $ Preturb.assertNotPreturb1XSameSemanticsX @a RF.zero
        , testCase "-zero" $
            Preturb.assertNotPreturb1XSameSemanticsX @a RF.negZero
        , testCase "min" $
            Preturb.assertNotPreturb1XSameSemanticsX @a RF.minValue
        , testCase "max" $
            Preturb.assertNotPreturb1XSameSemanticsX @a RF.maxValue
        , testCase "inf" $ Preturb.assertNotPreturb1XSameSemanticsX @a RF.inf
        , testCase "-inf" $
            Preturb.assertNotPreturb1XSameSemanticsX @a RF.negInf
        , testCase "NaN" $ Preturb.assertNotPreturb1XSameSemanticsX @a RF.nan
        , testProperty "random" $ Preturb.prop_NotPreturb1XSameSemanticsX @a
        ]
    , testGroup Preturb.namePreturbNegateNPreturbNXSameSemanticsX
        [ testCase "size:max" $
            Preturb.assertPreturbNegateNPreturbNXSameSemanticsX @a maxBound x
        , testProperty "random" $
            Preturb.prop_PreturbNegateNPreturbNXSameSemanticsX @a
        ]
    , testGroup Preturb.nameHsPreturbNXSameSemanticsCPreturbNX
        [ testCase "zero" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 RF.zero
        , testCase "-zero" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 RF.negZero
        , testCase "min" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 RF.minValue
        , testCase "max" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 RF.maxValue
        , testCase "inf" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 RF.inf
        , testCase "-inf" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 RF.negInf
        , testCase "NaN" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 1 RF.nan
        , testCase "size:zero" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX 0 x
        , testCase "size:min" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX minBound x
        , testCase "size:max" $
            Preturb.assertHsPreturbNXSameSemanticsCPreturbNX cX maxBound x
        , testProperty "random" $
            Preturb.prop_HsPreturbNXSameSemanticsCPreturbNX cX
        ]
    ]
  where
    x :: a
    x = 11

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestRuntime.Preturb"
    [ testBoundedNum CLib.preturbCChar
    , testBoundedNum CLib.preturbCSChar
    , testBoundedNum CLib.preturbCUChar
    , testBoundedNum CLib.preturbCShort
    , testBoundedNum CLib.preturbCUShort
    , testBoundedNum CLib.preturbCInt
    , testBoundedNum CLib.preturbCUInt
    , testBoundedNum CLib.preturbCLong
    , testBoundedNum CLib.preturbCULong
    , testBoundedNum CLib.preturbCPtrdiff
    , testBoundedNum CLib.preturbCSize
    , testBoundedNum CLib.preturbCWchar
    , testBoundedNum CLib.preturbCSigAtomic
    , testBoundedNum CLib.preturbCLLong
    , testBoundedNum CLib.preturbCULLong
    , testBoundedNum CLib.preturbCBool
    , testBoundedNum CLib.preturbCIntPtr
    , testBoundedNum CLib.preturbCUIntPtr
    , testBoundedNum CLib.preturbCIntMax
    , testBoundedNum CLib.preturbCUIntMax
    , testNum        CLib.preturbCClock
    , testNum        CLib.preturbCTime
    , testRealFloat  CLib.preturbCFloat
    , testRealFloat  CLib.preturbCDouble
    ]
