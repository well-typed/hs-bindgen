module HsBindgen.TestLib.Preturb.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestLib.Arbitrary ()
import HsBindgen.TestLib.CLib qualified as CLib
import HsBindgen.TestLib.RealFloat qualified as RF
import HsBindgen.TestLib.SameSemantics (SameSemantics)
import HsBindgen.TestLib.Preturb
  ( Preturb, assertPreturbHsSameSemanticsC
  , assertPreturbVNotSameSemanticsV, prop_PreturbVNotSameSemanticsV
  , prop_PreturbHsSameSemanticsC
  )

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a 'Num'
testNum :: forall a.
     (Arbitrary a, Num a, Preturb a, SameSemantics a, Show a, Typeable a)
  => (a -> IO a)
  -> TestTree
testNum cPreturb = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "PreturbVNotSameSemanticsV"
        [ testCase "zero" $ assertPreturbVNotSameSemanticsV @a 0
        , testProperty "random" $ prop_PreturbVNotSameSemanticsV @a
        ]
    , testGroup "PreturbHsSameSemanticsC"
        [ testCase "zero" $ assertPreturbHsSameSemanticsC cPreturb 0
        , testProperty "random" $ prop_PreturbHsSameSemanticsC cPreturb
        ]
    ]

-- | Test a 'Bounded' 'Num'
testBoundedNum :: forall a.
     ( Arbitrary a
     , Bounded a
     , Preturb a
     , Num a
     , SameSemantics a
     , Show a
     , Typeable a
     )
  => (a -> IO a)
  -> TestTree
testBoundedNum cPreturb = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "PreturbVNotSameSemanticsV"
        [ testCase "zero" $ assertPreturbVNotSameSemanticsV @a 0
        , testCase "min"  $ assertPreturbVNotSameSemanticsV @a minBound
        , testCase "max"  $ assertPreturbVNotSameSemanticsV @a maxBound
        , testProperty "random" $ prop_PreturbVNotSameSemanticsV @a
        ]
    , testGroup "PreturbHsSameSemanticsC"
        [ testCase "zero" $ assertPreturbHsSameSemanticsC cPreturb 0
        , testCase "min"  $ assertPreturbHsSameSemanticsC cPreturb minBound
        , testCase "max"  $ assertPreturbHsSameSemanticsC cPreturb maxBound
        , testProperty "random" $ prop_PreturbHsSameSemanticsC cPreturb
        ]
    ]

-- | Test a 'RealFloat'
testRealFloat :: forall a.
     ( Arbitrary a
     , Preturb a
     , RealFloat a
     , SameSemantics a
     , Show a
     , Typeable a
     )
  => (a -> IO a)
  -> TestTree
testRealFloat cPreturb = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "PreturbVNotSameSemanticsV"
        [ testCase "zero"  $ assertPreturbVNotSameSemanticsV @a RF.zero
        , testCase "-zero" $ assertPreturbVNotSameSemanticsV @a RF.negZero
        , testCase "min"   $ assertPreturbVNotSameSemanticsV @a RF.minValue
        , testCase "max"   $ assertPreturbVNotSameSemanticsV @a RF.maxValue
        , testCase "inf"   $ assertPreturbVNotSameSemanticsV @a RF.inf
        , testCase "-inf"  $ assertPreturbVNotSameSemanticsV @a RF.negInf
        , testCase "NaN"   $ assertPreturbVNotSameSemanticsV @a RF.nan
        , testProperty "random" $ prop_PreturbVNotSameSemanticsV @a
        ]
    , testGroup "PreturbHsSameSemanticsC"
        [ testCase "zero" $ assertPreturbHsSameSemanticsC cPreturb RF.zero
        , testCase "-zero" $
            assertPreturbHsSameSemanticsC cPreturb RF.negZero
        , testCase "min" $
            assertPreturbHsSameSemanticsC cPreturb RF.minValue
        , testCase "max" $
            assertPreturbHsSameSemanticsC cPreturb RF.maxValue
        , testCase "inf" $ assertPreturbHsSameSemanticsC cPreturb RF.inf
        , testCase "-inf" $ assertPreturbHsSameSemanticsC cPreturb RF.negInf
        , testCase "NaN" $ assertPreturbHsSameSemanticsC cPreturb RF.nan
        , testProperty "random" $ prop_PreturbHsSameSemanticsC cPreturb
        ]
    ]

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestLib.Preturb"
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
