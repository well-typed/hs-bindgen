{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Orphans () where

import Control.Exception (Exception (displayException))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.GADT.Compare (GEq (geq))
import Data.Text qualified as Text
import Data.Type.Equality ((:~:) (Refl))
import DeBruijn.Idx (Idx, idxToInt)
import Unsafe.Coerce (unsafeCoerce)

import HsBindgen.Frontend.RootHeader

{-------------------------------------------------------------------------------
  Aeson
-------------------------------------------------------------------------------}

instance Aeson.FromJSON HashIncludeArg where
  parseJSON = Aeson.withText "HashIncludeArg" $
      either (Aeson.parseFail . displayException) return
    . parseHashIncludeArg
    . Text.unpack

instance Aeson.ToJSON HashIncludeArg where
  toJSON = Aeson.String . Text.pack . renderHashIncludeArg

{-------------------------------------------------------------------------------
  DeBruijn
-------------------------------------------------------------------------------}

instance GEq Idx where
  geq i j
    | idxToInt i == idxToInt j = Just $ unsafeCoerce Refl
    | otherwise                = Nothing
