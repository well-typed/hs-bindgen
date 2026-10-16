{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data EnumA

instance Eq EnumA

instance BG.Generic EnumA

instance Ord EnumA

instance BG.HasFFIType EnumA

instance Marshal.StaticSize EnumA

instance Marshal.ReadRaw EnumA

instance Marshal.WriteRaw EnumA

instance BG.Storable EnumA

instance BG.Prim EnumA

instance CEnum.CEnum EnumA

instance CEnum.SequentialCEnum EnumA

instance Show EnumA

instance Read EnumA

data ExA

instance Eq ExA

instance BG.Generic ExA

instance Show ExA

instance Marshal.StaticSize ExA

instance Marshal.ReadRaw ExA

instance Marshal.WriteRaw ExA

instance BG.Storable ExA

instance Struct.IsStruct ExA

data ExB_fieldB1

instance Eq ExB_fieldB1

instance BG.Generic ExB_fieldB1

instance Ord ExB_fieldB1

instance BG.HasFFIType ExB_fieldB1

instance Marshal.StaticSize ExB_fieldB1

instance Marshal.ReadRaw ExB_fieldB1

instance Marshal.WriteRaw ExB_fieldB1

instance BG.Storable ExB_fieldB1

instance BG.Prim ExB_fieldB1

instance CEnum.CEnum ExB_fieldB1

instance CEnum.SequentialCEnum ExB_fieldB1

instance Show ExB_fieldB1

instance Read ExB_fieldB1

data ExB

instance Eq ExB

instance BG.Generic ExB

instance Show ExB

instance Marshal.StaticSize ExB

instance Marshal.ReadRaw ExB

instance Marshal.WriteRaw ExB

instance BG.Storable ExB

instance Struct.IsStruct ExB
