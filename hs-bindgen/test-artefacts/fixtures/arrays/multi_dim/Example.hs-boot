{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Matrix

instance Eq Matrix

instance BG.Generic Matrix

instance Show Matrix

instance IsA.IsArray Matrix

instance Marshal.ReadRaw Matrix

instance Marshal.StaticSize Matrix

instance BG.Storable Matrix

instance Marshal.WriteRaw Matrix

data Triplets

instance Eq Triplets

instance BG.Generic Triplets

instance Show Triplets

instance IsA.IsArray Triplets
