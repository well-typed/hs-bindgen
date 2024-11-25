module HsBindgen.TestLib.Storable (
    -- * Properties
    namePokePeekXSameSemanticsX
  , prop_PokePeekXSameSemanticsX
  , assertPokePeekXSameSemanticsX
  ) where

import Foreign qualified as F
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.QuickCheck (Property)

import HsBindgen.TestLib.SameSemantics ((@==~?), SameSemantics(sameSemantics))

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

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
