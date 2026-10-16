{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Triplet

instance Eq Triplet

instance BG.Generic Triplet

instance Show Triplet

instance IsA.IsArray Triplet

instance Marshal.ReadRaw Triplet

instance Marshal.StaticSize Triplet

instance BG.Storable Triplet

instance Marshal.WriteRaw Triplet
