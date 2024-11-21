module HsBindgen.TestLib.Storable (
    -- * Properties
    prop_PokePeekSameSemantics
  ) where

import Foreign qualified as F
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty.QuickCheck (Property)

import HsBindgen.TestLib.SameSemantics (SameSemantics(sameSemantics))

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | A value that is `F.poke`ed and then `F.peek`ed is semantically equal to the
-- orignal value
prop_PokePeekSameSemantics :: (F.Storable a, SameSemantics a) => a -> Property
prop_PokePeekSameSemantics x = QCM.monadicIO $ do
    x' <- QCM.run . F.alloca $ \ptr -> do
      F.poke ptr x
      F.peek ptr
    QCM.assert $ x' `sameSemantics` x
