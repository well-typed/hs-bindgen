module HsBindgen.TestRuntime.Perturb.Test (tests) where

import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestRuntime.Arbitrary ()
import HsBindgen.TestRuntime.CLib qualified as CLib
import HsBindgen.TestRuntime.Perturb (Perturb)
import HsBindgen.TestRuntime.Perturb qualified as Perturb
import HsBindgen.TestRuntime.RealFloat qualified as RF
import HsBindgen.TestRuntime.SameSemantics (SameSemantics)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a 'Num'
testNum :: forall a.
     (Arbitrary a, Num a, Perturb a, SameSemantics a, Show a, Typeable a)
  => (FC.CLLong -> a -> IO a)
  -> TestTree
testNum cX = testGroup (show (typeRep (Proxy @a)))
    [ testGroup Perturb.namePerturb0XSameSemanticsX
        [ testCase "zero" $ Perturb.assertPerturb0XSameSemanticsX @a 0
        , testProperty "random" $ Perturb.prop_Perturb0XSameSemanticsX @a
        ]
    , testGroup Perturb.nameNotPerturb1XSameSemanticsX
        [ testCase "zero" $ Perturb.assertNotPerturb1XSameSemanticsX @a 0
        , testProperty "random" $ Perturb.prop_NotPerturb1XSameSemanticsX @a
        ]
    , testGroup Perturb.namePerturbNegateNPerturbNXSameSemanticsX
        [ testProperty "random" $
            Perturb.prop_PerturbNegateNPerturbNXSameSemanticsX @a
        ]
    , testGroup Perturb.nameHsPerturbNXSameSemanticsCPerturbNX
        [ testCase "size:zero" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 0 x
        , testProperty "random" $
            Perturb.prop_HsPerturbNXSameSemanticsCPerturbNX cX
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
     , Perturb a
     , SameSemantics a
     , Show a
     , Typeable a
     )
  => (FC.CLLong -> a -> IO a)
  -> TestTree
testBoundedNum cX = testGroup (show (typeRep (Proxy @a)))
    [ testGroup Perturb.namePerturb0XSameSemanticsX
        [ testCase "zero" $ Perturb.assertPerturb0XSameSemanticsX @a 0
        , testCase "min" $ Perturb.assertPerturb0XSameSemanticsX @a minBound
        , testCase "max" $ Perturb.assertPerturb0XSameSemanticsX @a maxBound
        , testProperty "random" $ Perturb.prop_Perturb0XSameSemanticsX @a
        ]
    , testGroup Perturb.nameNotPerturb1XSameSemanticsX
        [ testCase "zero" $ Perturb.assertNotPerturb1XSameSemanticsX @a 0
        , testCase "min" $ Perturb.assertNotPerturb1XSameSemanticsX @a minBound
        , testCase "max" $ Perturb.assertNotPerturb1XSameSemanticsX @a maxBound
        , testProperty "random" $ Perturb.prop_NotPerturb1XSameSemanticsX @a
        ]
    , testGroup Perturb.namePerturbNegateNPerturbNXSameSemanticsX
        [ testCase "size:max" $
            Perturb.assertPerturbNegateNPerturbNXSameSemanticsX @a maxBound x
        , testProperty "random" $
            Perturb.prop_PerturbNegateNPerturbNXSameSemanticsX @a
        ]
    , testGroup Perturb.nameHsPerturbNXSameSemanticsCPerturbNX
        [ testCase "zero" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 0
        , testCase "min" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 minBound
        , testCase "max" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 maxBound
        , testCase "size:zero" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 0 x
        , testCase "size:min" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX minBound x
        , testCase "size:max" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX maxBound x
        , testProperty "random" $
            Perturb.prop_HsPerturbNXSameSemanticsCPerturbNX cX
        ]
    ]
  where
    x :: a
    x = 11

-- | Test a 'RealFloat'
testRealFloat :: forall a.
     ( Arbitrary a
     , Perturb a
     , RealFloat a
     , SameSemantics a
     , Show a
     , Typeable a
     )
  => (FC.CLLong -> a -> IO a)
  -> TestTree
testRealFloat cX = testGroup (show (typeRep (Proxy @a)))
    [ testGroup Perturb.namePerturb0XSameSemanticsX
        [ testCase "zero" $ Perturb.assertPerturb0XSameSemanticsX @a RF.zero
        , testCase "-zero" $ Perturb.assertPerturb0XSameSemanticsX @a RF.negZero
        , testCase "min" $ Perturb.assertPerturb0XSameSemanticsX @a RF.minValue
        , testCase "max" $ Perturb.assertPerturb0XSameSemanticsX @a RF.maxValue
        , testCase "inf" $ Perturb.assertPerturb0XSameSemanticsX @a RF.inf
        , testCase "-inf" $ Perturb.assertPerturb0XSameSemanticsX @a RF.negInf
        , testCase "NaN" $ Perturb.assertPerturb0XSameSemanticsX @a RF.nan
        , testProperty "random" $ Perturb.prop_Perturb0XSameSemanticsX @a
        ]
    , testGroup Perturb.nameNotPerturb1XSameSemanticsX
        [ testCase "zero" $ Perturb.assertNotPerturb1XSameSemanticsX @a RF.zero
        , testCase "-zero" $
            Perturb.assertNotPerturb1XSameSemanticsX @a RF.negZero
        , testCase "min" $
            Perturb.assertNotPerturb1XSameSemanticsX @a RF.minValue
        , testCase "max" $
            Perturb.assertNotPerturb1XSameSemanticsX @a RF.maxValue
        , testCase "inf" $ Perturb.assertNotPerturb1XSameSemanticsX @a RF.inf
        , testCase "-inf" $
            Perturb.assertNotPerturb1XSameSemanticsX @a RF.negInf
        , testCase "NaN" $ Perturb.assertNotPerturb1XSameSemanticsX @a RF.nan
        , testProperty "random" $ Perturb.prop_NotPerturb1XSameSemanticsX @a
        ]
    , testGroup Perturb.namePerturbNegateNPerturbNXSameSemanticsX
        [ testCase "size:max" $
            Perturb.assertPerturbNegateNPerturbNXSameSemanticsX @a maxBound x
        , testProperty "random" $
            Perturb.prop_PerturbNegateNPerturbNXSameSemanticsX @a
        ]
    , testGroup Perturb.nameHsPerturbNXSameSemanticsCPerturbNX
        [ testCase "zero" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 RF.zero
        , testCase "-zero" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 RF.negZero
        , testCase "min" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 RF.minValue
        , testCase "max" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 RF.maxValue
        , testCase "inf" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 RF.inf
        , testCase "-inf" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 RF.negInf
        , testCase "NaN" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 1 RF.nan
        , testCase "size:zero" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX 0 x
        , testCase "size:min" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX minBound x
        , testCase "size:max" $
            Perturb.assertHsPerturbNXSameSemanticsCPerturbNX cX maxBound x
        , testProperty "random" $
            Perturb.prop_HsPerturbNXSameSemanticsCPerturbNX cX
        ]
    ]
  where
    x :: a
    x = 11

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestRuntime.Perturb"
    [ testBoundedNum CLib.perturbCChar
    , testBoundedNum CLib.perturbCSChar
    , testBoundedNum CLib.perturbCUChar
    , testBoundedNum CLib.perturbCShort
    , testBoundedNum CLib.perturbCUShort
    , testBoundedNum CLib.perturbCInt
    , testBoundedNum CLib.perturbCUInt
    , testBoundedNum CLib.perturbCLong
    , testBoundedNum CLib.perturbCULong
    , testBoundedNum CLib.perturbCPtrdiff
    , testBoundedNum CLib.perturbCSize
    , testBoundedNum CLib.perturbCWchar
    , testBoundedNum CLib.perturbCSigAtomic
    , testBoundedNum CLib.perturbCLLong
    , testBoundedNum CLib.perturbCULLong
    , testBoundedNum CLib.perturbCBool
    , testBoundedNum CLib.perturbCIntPtr
    , testBoundedNum CLib.perturbCUIntPtr
    , testBoundedNum CLib.perturbCIntMax
    , testBoundedNum CLib.perturbCUIntMax
    , testNum        CLib.perturbCClock
    , testNum        CLib.perturbCTime
    , testRealFloat  CLib.perturbCFloat
    , testRealFloat  CLib.perturbCDouble
    ]
