{-# OPTIONS_GHC -Wno-orphans #-}

module HsBindgen.TestLib.Arbitrary () where

import Foreign.C qualified as FC
import Test.QuickCheck

{-------------------------------------------------------------------------------
  Orphan instances
-------------------------------------------------------------------------------}

instance Arbitrary FC.CBool where
  arbitrary = chooseEnum (0, 1)
  shrink = \case
    0 -> []
    _ -> [0]
