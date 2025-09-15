module HsBindgen.TestRuntime.Storable.Test (tests) where

import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, typeRep)
import Foreign qualified as F
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestRuntime.Arbitrary ()
import HsBindgen.TestRuntime.CLib qualified as CLib
import HsBindgen.TestRuntime.RealFloat qualified as RF
import HsBindgen.TestRuntime.SameSemantics (SameSemantics)
import HsBindgen.TestRuntime.Storable qualified as Storable

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a 'Num'
testNum :: forall a.
     (Arbitrary a, F.Storable a, Num a, SameSemantics a, Show a, Typeable a)
  => Proxy a
  -> IO FC.CSize
  -> IO FC.CSize
  -> TestTree
testNum proxy cSizeOf cAlignOf = testGroup (show (typeRep proxy))
    [ testCase Storable.nameHsSizeOfXEqCSizeOfX $
        Storable.assertHsSizeOfXEqCSizeOfX proxy cSizeOf
    , testCase Storable.nameHsAlignOfXEqCAlignOfX $
        Storable.assertHsAlignOfXEqCAlignOfX proxy cAlignOf
    , testGroup Storable.namePokePeekXSameSemanticsX
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
  -> IO FC.CSize
  -> IO FC.CSize
  -> TestTree
testBoundedNum proxy cSizeOf cAlignOf = testGroup (show (typeRep proxy))
    [ testCase Storable.nameHsSizeOfXEqCSizeOfX $
        Storable.assertHsSizeOfXEqCSizeOfX proxy cSizeOf
    , testCase Storable.nameHsAlignOfXEqCAlignOfX $
        Storable.assertHsAlignOfXEqCAlignOfX proxy cAlignOf
    , testGroup Storable.namePokePeekXSameSemanticsX
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
  -> IO FC.CSize
  -> IO FC.CSize
  -> TestTree
testRealFloat proxy cSizeOf cAlignOf = testGroup (show (typeRep proxy))
    [ testCase Storable.nameHsSizeOfXEqCSizeOfX $
        Storable.assertHsSizeOfXEqCSizeOfX proxy cSizeOf
    , testCase Storable.nameHsAlignOfXEqCAlignOfX $
        Storable.assertHsAlignOfXEqCAlignOfX proxy cAlignOf
    , testGroup Storable.namePokePeekXSameSemanticsX
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
tests = testGroup "HsBindgen.TestRuntime.Storable"
    [ testBoundedNum @FC.CChar Proxy CLib.sizeofCChar CLib.alignofCChar
    , testBoundedNum @FC.CSChar Proxy CLib.sizeofCSChar CLib.alignofCSChar
    , testBoundedNum @FC.CUChar Proxy CLib.sizeofCUChar CLib.alignofCUChar
    , testBoundedNum @FC.CShort Proxy CLib.sizeofCShort CLib.alignofCShort
    , testBoundedNum @FC.CUShort Proxy CLib.sizeofCUShort CLib.alignofCUShort
    , testBoundedNum @FC.CInt Proxy CLib.sizeofCInt CLib.alignofCInt
    , testBoundedNum @FC.CUInt Proxy CLib.sizeofCUInt CLib.alignofCUInt
    , testBoundedNum @FC.CLong Proxy CLib.sizeofCLong CLib.alignofCLong
    , testBoundedNum @FC.CULong Proxy CLib.sizeofCULong CLib.alignofCULong
    , testBoundedNum @FC.CPtrdiff Proxy CLib.sizeofCPtrdiff CLib.alignofCPtrdiff
    , testBoundedNum @FC.CSize Proxy CLib.sizeofCSize CLib.alignofCSize
    , testBoundedNum @FC.CWchar Proxy CLib.sizeofCWchar CLib.alignofCWchar
    , testBoundedNum
        @FC.CSigAtomic
        Proxy
        CLib.sizeofCSigAtomic
        CLib.alignofCSigAtomic
    , testBoundedNum @FC.CLLong Proxy CLib.sizeofCLLong CLib.alignofCLLong
    , testBoundedNum @FC.CULLong Proxy CLib.sizeofCULLong CLib.alignofCULLong
    , testBoundedNum @FC.CBool Proxy CLib.sizeofCBool CLib.alignofCBool
    , testBoundedNum @FC.CIntPtr Proxy CLib.sizeofCIntPtr CLib.alignofCIntPtr
    , testBoundedNum @FC.CUIntPtr Proxy CLib.sizeofCUIntPtr CLib.alignofCUIntPtr
    , testBoundedNum @FC.CIntMax Proxy CLib.sizeofCIntMax CLib.alignofCIntMax
    , testBoundedNum @FC.CUIntMax Proxy CLib.sizeofCUIntMax CLib.alignofCUIntMax
    , testNum @FC.CClock Proxy CLib.sizeofCClock CLib.alignofCClock
    , testNum @FC.CTime Proxy CLib.sizeofCTime CLib.alignofCTime
    , testRealFloat @FC.CFloat Proxy CLib.sizeofCFloat CLib.alignofCFloat
    , testRealFloat @FC.CDouble Proxy CLib.sizeofCDouble CLib.alignofCDouble
    ]
