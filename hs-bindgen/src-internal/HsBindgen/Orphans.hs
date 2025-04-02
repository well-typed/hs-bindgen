{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Orphans () where

import Control.Exception (Exception(displayException))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.GADT.Compare (GEq(geq))
import Data.Text qualified as Text
import Data.Type.Equality ((:~:)(Refl))
import DeBruijn.Idx (Idx, idxToInt)
import Unsafe.Coerce (unsafeCoerce)

import Clang.CNameSpelling
import Clang.Paths

{-------------------------------------------------------------------------------
  Aeson
-------------------------------------------------------------------------------}

instance Aeson.FromJSON CHeaderIncludePath where
  parseJSON = Aeson.withText "CHeaderIncludePath" $
      either (Aeson.parseFail . displayException) return
    . parseCHeaderIncludePath
    . Text.unpack

instance Aeson.ToJSON CHeaderIncludePath where
  toJSON = Aeson.String . Text.pack . renderCHeaderIncludePath

deriving newtype instance Aeson.FromJSON CNameSpelling

deriving newtype instance Aeson.ToJSON CNameSpelling

{-------------------------------------------------------------------------------
  DeBruijn
-------------------------------------------------------------------------------}

instance GEq Idx where
  geq i j
    | idxToInt i == idxToInt j = Just $ unsafeCoerce Refl
    | otherwise                = Nothing
