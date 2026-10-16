{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Vector

instance Eq Vector

instance BG.Generic Vector

instance Show Vector

instance Marshal.StaticSize Vector

instance Marshal.ReadRaw Vector

instance Marshal.WriteRaw Vector

instance BG.Storable Vector

instance Struct.IsStruct Vector
