{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans () where

import Foreign.C.Types (CInt (..))
import GHC.Generics (Generic)
import Test.Tasty.QuickCheck (CoArbitrary, Function)

deriving stock instance Generic CInt
deriving anyclass instance Function CInt
deriving anyclass instance CoArbitrary CInt
