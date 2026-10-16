{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data FileOperationStatus

instance Eq FileOperationStatus

instance BG.Generic FileOperationStatus

instance Ord FileOperationStatus

instance BG.HasFFIType FileOperationStatus

instance Marshal.StaticSize FileOperationStatus

instance Marshal.ReadRaw FileOperationStatus

instance Marshal.WriteRaw FileOperationStatus

instance BG.Storable FileOperationStatus

instance BG.Prim FileOperationStatus

instance CEnum.CEnum FileOperationStatus

instance Show FileOperationStatus

instance Read FileOperationStatus

data FileOperationRecord

instance Eq FileOperationRecord

instance BG.Generic FileOperationRecord

instance Show FileOperationRecord

instance Marshal.StaticSize FileOperationRecord

instance Marshal.ReadRaw FileOperationRecord

instance Marshal.WriteRaw FileOperationRecord

instance BG.Storable FileOperationRecord

instance Struct.IsStruct FileOperationRecord
