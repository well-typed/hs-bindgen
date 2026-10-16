{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Hash_defines_buffer

instance Eq Hash_defines_buffer

instance BG.Generic Hash_defines_buffer

instance Show Hash_defines_buffer

instance Marshal.StaticSize Hash_defines_buffer

instance Marshal.ReadRaw Hash_defines_buffer

instance Marshal.WriteRaw Hash_defines_buffer

instance BG.Storable Hash_defines_buffer

instance Struct.IsStruct Hash_defines_buffer
