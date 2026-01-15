{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans (
  ) where

import HsBindgen.Runtime.SizedByteArray

deriving stock instance Show (SizedByteArray n m)
deriving stock instance Eq   (SizedByteArray n m)
