module HsBindgen.TestRuntime.Storable (
    -- * Properties
    nameHsSizeOfXEqCSizeOfX
  , assertHsSizeOfXEqCSizeOfX
  , nameHsAlignOfXEqCAlignOfX
  , assertHsAlignOfXEqCAlignOfX
  , namePokePeekXSameSemanticsX
  , prop_PokePeekXSameSemanticsX
  , assertPokePeekXSameSemanticsX
  ) where

import Data.Proxy (Proxy)
import Foreign qualified as F
import Foreign.C qualified as FC
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty.HUnit ((@=?), Assertion)
import Test.Tasty.QuickCheck (Property)

import HsBindgen.TestRuntime.SameSemantics (
    (@==~?), SameSemantics(sameSemantics)
  )

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | The size of a type is the same in Haskell and C
nameHsSizeOfXEqCSizeOfX :: String
nameHsSizeOfXEqCSizeOfX = "HsSizeOfXEqCSizeOfX"

-- | The size of a type is the same in Haskell and C
assertHsSizeOfXEqCSizeOfX :: forall a.
     F.Storable a
  => Proxy a
  -> IO FC.CSize
  -> Assertion
assertHsSizeOfXEqCSizeOfX _proxy cSizeOfX = do
    cSizeOf <- fromIntegral <$> cSizeOfX
    F.sizeOf @a undefined @=? cSizeOf

-- | The alignment of a type is the same in Haskell and C
nameHsAlignOfXEqCAlignOfX :: String
nameHsAlignOfXEqCAlignOfX = "HsAlignOfXEqCAlignOfX"

-- | The alignment of a type is the same in Haskell and C
assertHsAlignOfXEqCAlignOfX :: forall a.
     F.Storable a
  => Proxy a
  -> IO FC.CSize
  -> Assertion
assertHsAlignOfXEqCAlignOfX _proxy cAlignOfX = do
    cAlignOf <- fromIntegral <$> cAlignOfX
    F.alignment @a undefined @=? cAlignOf

-- | The 'F.poke' then 'F.peek' of a value is semantically equal to the value
namePokePeekXSameSemanticsX :: String
namePokePeekXSameSemanticsX = "PokePeekXSameSemanticsX"

-- | The 'F.poke' then 'F.peek' of a value is semantically equal to the value
prop_PokePeekXSameSemanticsX :: (F.Storable a, SameSemantics a) => a -> Property
prop_PokePeekXSameSemanticsX x = QCM.monadicIO $ do
    x' <- QCM.run . F.alloca $ \ptr -> do
      F.poke ptr x
      F.peek ptr
    QCM.assert $ x' `sameSemantics` x

-- | The 'F.poke' then 'F.peek' of a value is semantically equal to the value
assertPokePeekXSameSemanticsX ::
       (F.Storable a, SameSemantics a, Show a)
    => a
    -> Assertion
assertPokePeekXSameSemanticsX x = do
    x' <- F.alloca $ \ptr -> do
      F.poke ptr x
      F.peek ptr
    x @==~? x'
