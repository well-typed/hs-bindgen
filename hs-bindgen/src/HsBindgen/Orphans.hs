{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Orphans () where

import Data.Aeson qualified as Aeson
import Data.GADT.Compare (GEq(geq))
import Data.Type.Equality ((:~:)(Refl))
import DeBruijn.Idx (Idx, idxToInt)
import Unsafe.Coerce (unsafeCoerce)

import HsBindgen.Clang.CNameSpelling
import HsBindgen.Clang.Paths

{-------------------------------------------------------------------------------
  Aeson
-------------------------------------------------------------------------------}

instance Aeson.FromJSON CHeaderRelPath where
  parseJSON = Aeson.withText "CHeaderRelPath" mkCHeaderRelPath

deriving newtype instance Aeson.FromJSON CNameSpelling

{-------------------------------------------------------------------------------
  DeBruijn
-------------------------------------------------------------------------------}

instance GEq Idx where
  geq i j
    | idxToInt i == idxToInt j = Just $ unsafeCoerce Refl
    | otherwise                = Nothing
