{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data NewName

instance Eq NewName

instance BG.Generic NewName

instance Show NewName

instance Marshal.StaticSize NewName

instance Marshal.ReadRaw NewName

instance Marshal.WriteRaw NewName

instance BG.Storable NewName

instance Struct.IsStruct NewName
