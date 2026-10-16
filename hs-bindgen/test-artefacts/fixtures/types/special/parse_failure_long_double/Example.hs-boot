{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Struct2

instance Eq Struct2

instance BG.Generic Struct2

instance Show Struct2

instance Marshal.StaticSize Struct2

instance Marshal.ReadRaw Struct2

instance Marshal.WriteRaw Struct2

instance BG.Storable Struct2

instance Struct.IsStruct Struct2
