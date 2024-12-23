module HsBindgen.TestLib.GenSeq.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import HsBindgen.TestLib.CLib qualified as CLib
import HsBindgen.TestLib.GenSeq (GenSeq)
import HsBindgen.TestLib.GenSeq qualified as GenSeq
import HsBindgen.TestLib.SameSemantics (SameSemantics)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test any type
testA :: forall a.
     (GenSeq a, SameSemantics a, Show a, Typeable a)
  => (FC.CULong -> IO a)
  -> TestTree
testA cGenSeq = testGroup (show (typeRep (Proxy @a)))
    [ testGroup GenSeq.nameHsGenSeqNSameSemanticsCGenSeqN
        [ testCase "zero" $
            GenSeq.assertHsGenSeqNSameSemanticsCGenSeqN cGenSeq 0
        , testCase "max" $
            GenSeq.assertHsGenSeqNSameSemanticsCGenSeqN cGenSeq maxBound
        , testProperty "random" $
            GenSeq.prop_HsGenSeqNSameSemanticsCGenSeqN cGenSeq
        ]
    ]

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestLib.GenSeq"
    [ testA CLib.genSeqCChar
    , testA CLib.genSeqCSChar
    , testA CLib.genSeqCUChar
    , testA CLib.genSeqCShort
    , testA CLib.genSeqCUShort
    , testA CLib.genSeqCInt
    , testA CLib.genSeqCUInt
    , testA CLib.genSeqCLong
    , testA CLib.genSeqCULong
    , testA CLib.genSeqCPtrdiff
    , testA CLib.genSeqCSize
    , testA CLib.genSeqCWchar
    , testA CLib.genSeqCSigAtomic
    , testA CLib.genSeqCLLong
    , testA CLib.genSeqCULLong
    , testA CLib.genSeqCBool
    , testA CLib.genSeqCIntPtr
    , testA CLib.genSeqCUIntPtr
    , testA CLib.genSeqCIntMax
    , testA CLib.genSeqCUIntMax
    , testA CLib.genSeqCClock
    , testA CLib.genSeqCTime
    , testA CLib.genSeqCFloat
    , testA CLib.genSeqCDouble
    ]
