{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Orphans () where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.GADT.Compare (GEq(geq))
import Data.Text qualified as Text
import Data.Type.Equality ((:~:)(Refl))
import DeBruijn.Idx (Idx, idxToInt)
import Unsafe.Coerce (unsafeCoerce)

import HsBindgen.Clang.CNameSpelling
import HsBindgen.Clang.Paths

{-------------------------------------------------------------------------------
  Aeson
-------------------------------------------------------------------------------}

instance Aeson.FromJSON CHeaderIncludePath where
  parseJSON = Aeson.withText "CHeaderIncludePath" $
    either Aeson.parseFail return . parseCHeaderIncludePath . Text.unpack

deriving newtype instance Aeson.FromJSON CNameSpelling

{-------------------------------------------------------------------------------
  DeBruijn
-------------------------------------------------------------------------------}

instance GEq Idx where
  geq i j
    | idxToInt i == idxToInt j = Just $ unsafeCoerce Refl
    | otherwise                = Nothing
