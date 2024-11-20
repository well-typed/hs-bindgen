module HsBindgen.TestLib.Transform.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestLib.Arbitrary ()
import HsBindgen.TestLib.CLib qualified as CLib
import HsBindgen.TestLib.RealFloat qualified as RF
import HsBindgen.TestLib.RepEq (RepEq)
import HsBindgen.TestLib.Transform
  ( Transform, assertXHsRepEqXC, assertXvNotRepEqV, prop_XvNotRepEqV
  , prop_XHsRepEqXC
  )

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

testNum :: forall a.
     (Arbitrary a, Num a, RepEq a, Show a, Transform a, Typeable a)
  => (a -> IO a)
  -> TestTree
testNum cTransform = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "XvNotRepEqV"
        [ testCase     "zero"   $ assertXvNotRepEqV @a 0
        , testProperty "random" $ prop_XvNotRepEqV  @a
        ]
    , testGroup "XHsRepEqXC"
        [ testCase     "zero"   $ assertXHsRepEqXC cTransform 0
        , testProperty "random" $ prop_XHsRepEqXC  cTransform
        ]
    ]

-- | Test a 'Bounded' 'Num'
testBoundedNum :: forall a.
     (Arbitrary a, Bounded a, Num a, RepEq a, Show a, Transform a, Typeable a)
  => (a -> IO a)
  -> TestTree
testBoundedNum cTransform = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "XvNotRepEqV"
        [ testCase     "zero"   $ assertXvNotRepEqV @a 0
        , testCase     "min"    $ assertXvNotRepEqV @a minBound
        , testCase     "max"    $ assertXvNotRepEqV @a maxBound
        , testProperty "random" $ prop_XvNotRepEqV  @a
        ]
    , testGroup "XHsRepEqXC"
        [ testCase     "zero"   $ assertXHsRepEqXC cTransform 0
        , testCase     "min"    $ assertXHsRepEqXC cTransform minBound
        , testCase     "max"    $ assertXHsRepEqXC cTransform maxBound
        , testProperty "random" $ prop_XHsRepEqXC  cTransform
        ]
    ]

-- | Test a 'RealFloat'
testRealFloat :: forall a.
     (Arbitrary a, RealFloat a, RepEq a, Show a, Transform a, Typeable a)
  => (a -> IO a)
  -> TestTree
testRealFloat cTransform = testGroup (show (typeRep (Proxy @a)))
    [ testGroup "XvNotRepEqV"
        [ testCase "zero"       $ assertXvNotRepEqV @a RF.zero
        , testCase "-zero"      $ assertXvNotRepEqV @a RF.negZero
        , testCase "min"        $ assertXvNotRepEqV @a RF.minValue
        , testCase "max"        $ assertXvNotRepEqV @a RF.maxValue
        , testCase "infinity"   $ assertXvNotRepEqV @a RF.inf
        , testCase "-infinity"  $ assertXvNotRepEqV @a RF.negInf
        , testCase "NaN"        $ assertXvNotRepEqV @a RF.nan
        , testProperty "random" $ prop_XvNotRepEqV  @a
        ]
    , testGroup "XHsRepEqXC"
        [ testCase "zero"       $ assertXHsRepEqXC cTransform RF.zero
        , testCase "-zero"      $ assertXHsRepEqXC cTransform RF.negZero
        , testCase "min"        $ assertXHsRepEqXC cTransform RF.minValue
        , testCase "max"        $ assertXHsRepEqXC cTransform RF.maxValue
        , testCase "infinity"   $ assertXHsRepEqXC cTransform RF.inf
        , testCase "-infinity"  $ assertXHsRepEqXC cTransform RF.negInf
        , testCase "NaN"        $ assertXHsRepEqXC cTransform RF.nan
        , testProperty "random" $ prop_XHsRepEqXC  cTransform
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
    -- , testNum        CLib.transformCUSeconds
    , testNum        CLib.transformCSUSeconds
    , testRealFloat  CLib.transformCFloat
    , testRealFloat  CLib.transformCDouble
    ]
