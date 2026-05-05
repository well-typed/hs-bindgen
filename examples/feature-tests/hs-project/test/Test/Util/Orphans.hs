{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans () where

import Foreign.C.Types (CChar (..), CInt (..), CUChar (..), CUInt (..))
import GHC.Generics (Generic)
import Test.Tasty.QuickCheck (CoArbitrary, Function)

deriving stock instance Generic CInt
deriving anyclass instance Function CInt
deriving anyclass instance CoArbitrary CInt

deriving stock instance Generic CUInt
deriving anyclass instance Function CUInt
deriving anyclass instance CoArbitrary CUInt

deriving stock instance Generic CChar
deriving anyclass instance Function CChar
deriving anyclass instance CoArbitrary CChar

deriving stock instance Generic CUChar
deriving anyclass instance Function CUChar
deriving anyclass instance CoArbitrary CUChar
