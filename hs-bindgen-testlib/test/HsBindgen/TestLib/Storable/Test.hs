module HsBindgen.TestLib.Storable.Test (tests) where

import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(Proxy))
import Foreign qualified as F
import Foreign.C qualified as FC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import HsBindgen.TestLib.Arbitrary ()
import HsBindgen.TestLib.RepEq (RepEq)
import HsBindgen.TestLib.Storable (prop_PokePeekRepEq)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

-- | Test a general type
testGeneral :: forall a.
     (Arbitrary a, F.Storable a, RepEq a, Show a, Typeable a)
  => Proxy a
  -> TestTree
testGeneral proxy = testGroup (show (typeRep proxy))
    [ testGroup "PokePeekRepEq"
        [ testProperty "random" $ prop_PokePeekRepEq @a
        ]
    ]

{-------------------------------------------------------------------------------
  All tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.TestLib.Storable"
    [ testGeneral (Proxy @FC.CChar)
    , testGeneral (Proxy @FC.CSChar)
    , testGeneral (Proxy @FC.CUChar)
    , testGeneral (Proxy @FC.CShort)
    , testGeneral (Proxy @FC.CUShort)
    , testGeneral (Proxy @FC.CInt)
    , testGeneral (Proxy @FC.CUInt)
    , testGeneral (Proxy @FC.CLong)
    , testGeneral (Proxy @FC.CULong)
    , testGeneral (Proxy @FC.CPtrdiff)
    , testGeneral (Proxy @FC.CSize)
    , testGeneral (Proxy @FC.CWchar)
    , testGeneral (Proxy @FC.CSigAtomic)
    , testGeneral (Proxy @FC.CLLong)
    , testGeneral (Proxy @FC.CULLong)
    , testGeneral (Proxy @FC.CBool)
    , testGeneral (Proxy @FC.CIntPtr)
    , testGeneral (Proxy @FC.CUIntPtr)
    , testGeneral (Proxy @FC.CIntMax)
    , testGeneral (Proxy @FC.CUIntMax)
    , testGeneral (Proxy @FC.CClock)
    , testGeneral (Proxy @FC.CTime)
    -- , testGeneral (Proxy @FC.CUSeconds)
    , testGeneral (Proxy @FC.CSUSeconds)
    , testGeneral (Proxy @FC.CFloat)
    , testGeneral (Proxy @FC.CDouble)
    ]
