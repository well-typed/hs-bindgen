{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Opaque

data Outside

instance Eq Outside

instance BG.Generic Outside

instance Show Outside

instance Marshal.StaticSize Outside

instance Marshal.ReadRaw Outside

instance Marshal.WriteRaw Outside

instance BG.Storable Outside

instance Struct.IsStruct Outside
