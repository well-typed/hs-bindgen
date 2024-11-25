module HsBindgen.TestLib.Storable.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Foreign qualified as F
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestLib.Arbitrary ()
import HsBindgen.TestLib.RealFloat qualified as RF
import HsBindgen.TestLib.SameSemantics (SameSemantics)
import HsBindgen.TestLib.Storable qualified as Storable

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a 'Num'
testNum :: forall a.
     (Arbitrary a, F.Storable a, Num a, SameSemantics a, Show a, Typeable a)
  => Proxy a
  -> TestTree
testNum proxy = testGroup (show (typeRep proxy))
    [ testGroup Storable.namePokePeekXSameSemanticsX
        [ testCase "zero" $ Storable.assertPokePeekXSameSemanticsX @a 0
        , testProperty "random" $ Storable.prop_PokePeekXSameSemanticsX @a
        ]
    ]

-- | Test a 'Bounded' 'Num'
testBoundedNum :: forall a.
     ( Arbitrary a
     , Bounded a
     , F.Storable a
     , Num a
     , SameSemantics a
     , Show a
     , Typeable a
     )
  => Proxy a
  -> TestTree
testBoundedNum proxy = testGroup (show (typeRep proxy))
    [ testGroup Storable.namePokePeekXSameSemanticsX
        [ testCase "zero" $ Storable.assertPokePeekXSameSemanticsX @a 0
        , testCase "min" $ Storable.assertPokePeekXSameSemanticsX @a minBound
        , testCase "max" $ Storable.assertPokePeekXSameSemanticsX @a maxBound
        , testProperty "random" $ Storable.prop_PokePeekXSameSemanticsX @a
        ]
    ]

-- | Test a 'RealFloat'
testRealFloat :: forall a.
     ( Arbitrary a
     , F.Storable a
     , RealFloat a
     , SameSemantics a
     , Show a
     , Typeable a
     )
  => Proxy a
  -> TestTree
testRealFloat proxy = testGroup (show (typeRep proxy))
    [ testGroup Storable.namePokePeekXSameSemanticsX
        [ testCase "zero" $ Storable.assertPokePeekXSameSemanticsX @a RF.zero
        , testCase "-zero" $
            Storable.assertPokePeekXSameSemanticsX @a RF.negZero
        , testCase "min" $ Storable.assertPokePeekXSameSemanticsX @a RF.minValue
        , testCase "max" $ Storable.assertPokePeekXSameSemanticsX @a RF.maxValue
        , testCase "inf" $ Storable.assertPokePeekXSameSemanticsX @a RF.inf
        , testCase "-inf" $ Storable.assertPokePeekXSameSemanticsX @a RF.negInf
        , testCase "NaN" $ Storable.assertPokePeekXSameSemanticsX @a RF.nan
        , testProperty "random" $ Storable.prop_PokePeekXSameSemanticsX @a
        ]
    ]

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestLib.Storable"
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
