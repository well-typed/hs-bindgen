module Test.Util.QC (
    Arbitrary4 (..)
  ) where

import Data.Kind
import Test.QuickCheck

{-------------------------------------------------------------------------------
  Arbitrary4
-------------------------------------------------------------------------------}

class Arbitrary4 (f :: Type -> Type -> Type -> Type -> Type) where
  {-# MINIMAL liftArbitrary4 #-}
  liftArbitrary4 :: Gen a -> Gen b -> Gen c -> Gen d -> Gen (f a b c d)
  liftShrink4 :: (a -> [a]) -> (b -> [b]) -> (c -> [c]) -> (d -> [d]) -> f a b c d -> [f a b c d]
  liftShrink4 _ _ _ _ _ = []

instance Arbitrary4 (,,,) where
  liftArbitrary4 genA genB genC genD =
      (,,,) <$> genA <*> genB <*> genC <*> genD

  liftShrink4 shrA shrB shrC shrD (a, b, c, d) = do
      (a', (b', (c', d'))) <-
        liftShrink2 shrA (liftShrink2 shrB (liftShrink2 shrC shrD)) (a, (b, (c, d)))
      pure (a', b', c', d')
