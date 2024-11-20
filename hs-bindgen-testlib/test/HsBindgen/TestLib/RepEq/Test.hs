module HsBindgen.TestLib.RepEq.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestLib.Arbitrary ()
import HsBindgen.TestLib.RealFloat qualified as RF
import HsBindgen.TestLib.RepEq (RepEq, assertReflexive, prop_Reflexive)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a 'Num'
testNum :: forall a.
     (Arbitrary a, Num a, RepEq a, Show a, Typeable a)
  => Proxy a
  -> TestTree
testNum proxy = testGroup (show (typeRep proxy))
    [ testGroup "Reflexive"
        [ testCase     "zero"   $ assertReflexive @a 0
        , testProperty "random" $ prop_Reflexive  @a
        ]
    ]

-- | Test a 'Bounded' 'Num'
testBoundedNum :: forall a.
     (Arbitrary a, Bounded a, Num a, RepEq a, Show a, Typeable a)
  => Proxy a
  -> TestTree
testBoundedNum proxy = testGroup (show (typeRep proxy))
    [ testGroup "Reflexive"
        [ testCase     "zero"   $ assertReflexive @a 0
        , testCase     "min"    $ assertReflexive @a minBound
        , testCase     "max"    $ assertReflexive @a maxBound
        , testProperty "random" $ prop_Reflexive  @a
        ]
    ]

-- | Test a 'RealFloat'
testRealFloat :: forall a.
     (Arbitrary a, RealFloat a, RepEq a, Show a, Typeable a)
  => Proxy a
  -> TestTree
testRealFloat proxy = testGroup (show (typeRep proxy))
    [ testGroup "Reflexive"
        [ testCase     "zero"      $ assertReflexive @a RF.zero
        , testCase     "-zero"     $ assertReflexive @a RF.negZero
        , testCase     "min"       $ assertReflexive @a RF.minValue
        , testCase     "max"       $ assertReflexive @a RF.maxValue
        , testCase     "infinity"  $ assertReflexive @a RF.inf
        , testCase     "-infinity" $ assertReflexive @a RF.negInf
        , testCase     "NaN"       $ assertReflexive @a RF.nan
        , testProperty "random"    $ prop_Reflexive  @a
        ]
    ]

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestLib.RepEq"
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
