{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union

data UnionA

instance BG.Generic UnionA

instance Marshal.StaticSize UnionA

instance Marshal.ReadRaw UnionA

instance Marshal.WriteRaw UnionA

instance BG.Storable UnionA

instance Union.IsUnion UnionA

data ExA

instance BG.Generic ExA

instance Marshal.StaticSize ExA

instance Marshal.ReadRaw ExA

instance Marshal.WriteRaw ExA

instance BG.Storable ExA

instance Struct.IsStruct ExA

data ExB_fieldB1

instance BG.Generic ExB_fieldB1

instance Marshal.StaticSize ExB_fieldB1

instance Marshal.ReadRaw ExB_fieldB1

instance Marshal.WriteRaw ExB_fieldB1

instance BG.Storable ExB_fieldB1

instance Union.IsUnion ExB_fieldB1

data ExB

instance BG.Generic ExB

instance Marshal.StaticSize ExB

instance Marshal.ReadRaw ExB

instance Marshal.WriteRaw ExB

instance BG.Storable ExB

instance Struct.IsStruct ExB
