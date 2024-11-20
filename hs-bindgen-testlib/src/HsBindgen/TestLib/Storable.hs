module HsBindgen.TestLib.Storable (
    -- * Properties
    prop_PokePeekRepEq
  ) where

import Foreign qualified as F
import Test.QuickCheck.Monadic qualified as QCM
import Test.Tasty.QuickCheck (Property)

import HsBindgen.TestLib.RepEq (RepEq(repEq))

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

prop_PokePeekRepEq :: (F.Storable a, RepEq a) => a -> Property
prop_PokePeekRepEq x = QCM.monadicIO $ do
    x' <- QCM.run . F.alloca $ \ptr -> do
      F.poke ptr x
      F.peek ptr
    QCM.assert $ x' `repEq` x
