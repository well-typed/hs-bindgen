{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Some_struct_field1

instance Eq Some_struct_field1

instance BG.Generic Some_struct_field1

instance Show Some_struct_field1

instance Marshal.StaticSize Some_struct_field1

instance Marshal.ReadRaw Some_struct_field1

instance Marshal.WriteRaw Some_struct_field1

instance BG.Storable Some_struct_field1

instance Struct.IsStruct Some_struct_field1

data Some_struct

instance Eq Some_struct

instance BG.Generic Some_struct

instance Show Some_struct

instance Marshal.StaticSize Some_struct

instance Marshal.ReadRaw Some_struct

instance Marshal.WriteRaw Some_struct

instance BG.Storable Some_struct

instance Struct.IsStruct Some_struct
