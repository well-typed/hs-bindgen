module HsBindgen.TestLib.SameSemantics.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestLib.Arbitrary ()
import HsBindgen.TestLib.RealFloat qualified as RF
import HsBindgen.TestLib.SameSemantics (SameSemantics)
import HsBindgen.TestLib.SameSemantics qualified as SameSemantics

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a 'Num'
testNum :: forall a.
     (Arbitrary a, Num a, SameSemantics a, Show a, Typeable a)
  => Proxy a
  -> TestTree
testNum proxy = testGroup (show (typeRep proxy))
    [ testGroup SameSemantics.nameXSameSemanticsX
        [ testCase "zero" $ SameSemantics.assertXSameSemanticsX @a 0
        , testProperty "random" $ SameSemantics.prop_XSameSemanticsX  @a
        ]
    ]

-- | Test a 'Bounded' 'Num'
testBoundedNum :: forall a.
     (Arbitrary a, Bounded a, Num a, SameSemantics a, Show a, Typeable a)
  => Proxy a
  -> TestTree
testBoundedNum proxy = testGroup (show (typeRep proxy))
    [ testGroup SameSemantics.nameXSameSemanticsX
        [ testCase "zero" $ SameSemantics.assertXSameSemanticsX @a 0
        , testCase "min"  $ SameSemantics.assertXSameSemanticsX @a minBound
        , testCase "max"  $ SameSemantics.assertXSameSemanticsX @a maxBound
        , testProperty "random" $ SameSemantics.prop_XSameSemanticsX  @a
        ]
    ]

-- | Test a 'RealFloat'
testRealFloat :: forall a.
     (Arbitrary a, RealFloat a, SameSemantics a, Show a, Typeable a)
  => Proxy a
  -> TestTree
testRealFloat proxy = testGroup (show (typeRep proxy))
    [ testGroup SameSemantics.nameXSameSemanticsX
        [ testCase "zero"  $ SameSemantics.assertXSameSemanticsX @a RF.zero
        , testCase "-zero" $ SameSemantics.assertXSameSemanticsX @a RF.negZero
        , testCase "min"   $ SameSemantics.assertXSameSemanticsX @a RF.minValue
        , testCase "max"   $ SameSemantics.assertXSameSemanticsX @a RF.maxValue
        , testCase "inf"   $ SameSemantics.assertXSameSemanticsX @a RF.inf
        , testCase "-inf"  $ SameSemantics.assertXSameSemanticsX @a RF.negInf
        , testCase "NaN"   $ SameSemantics.assertXSameSemanticsX @a RF.nan
        , testProperty "random" $ SameSemantics.prop_XSameSemanticsX  @a
        ]
    ]

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestLib.SameSemantics"
    [ testBoundedNum (Proxy @FC.CChar)
    , testBoundedNum (Proxy @FC.CSChar)
    , testBoundedNum (Proxy @FC.CUChar)
    , testBoundedNum (Proxy @FC.CShort)
    , testBoundedNum (Proxy @FC.CUShort)
    , testBoundedNum (Proxy @FC.CInt)
    , testBoundedNum (Proxy @FC.CUInt)
    , testBoundedNum (Proxy @FC.CLong)
    , testBoundedNum (Proxy @FC.CULong)
    , testBoundedNum (Proxy @FC.CPtrdiff)
    , testBoundedNum (Proxy @FC.CSize)
    , testBoundedNum (Proxy @FC.CWchar)
    , testBoundedNum (Proxy @FC.CSigAtomic)
    , testBoundedNum (Proxy @FC.CLLong)
    , testBoundedNum (Proxy @FC.CULLong)
    , testBoundedNum (Proxy @FC.CBool)
    , testBoundedNum (Proxy @FC.CIntPtr)
    , testBoundedNum (Proxy @FC.CUIntPtr)
    , testBoundedNum (Proxy @FC.CIntMax)
    , testBoundedNum (Proxy @FC.CUIntMax)
    , testNum        (Proxy @FC.CClock)
    , testNum        (Proxy @FC.CTime)
    -- , testNum        (Proxy @FC.CUSeconds)
    , testNum        (Proxy @FC.CSUSeconds)
    , testRealFloat  (Proxy @FC.CFloat)
    , testRealFloat  (Proxy @FC.CDouble)
    ]
