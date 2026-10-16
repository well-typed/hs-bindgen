{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S
