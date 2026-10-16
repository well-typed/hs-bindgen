{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Omitted

instance Eq Omitted

instance BG.Generic Omitted

instance Show Omitted

instance Marshal.StaticSize Omitted

instance Marshal.ReadRaw Omitted

instance Marshal.WriteRaw Omitted

instance BG.Storable Omitted

instance Struct.IsStruct Omitted

data DirectlyDependsOnOmitted

instance Eq DirectlyDependsOnOmitted

instance BG.Generic DirectlyDependsOnOmitted

instance Show DirectlyDependsOnOmitted

instance Marshal.StaticSize DirectlyDependsOnOmitted

instance Marshal.ReadRaw DirectlyDependsOnOmitted

instance Marshal.WriteRaw DirectlyDependsOnOmitted

instance BG.Storable DirectlyDependsOnOmitted

instance Struct.IsStruct DirectlyDependsOnOmitted

data IndirectlyDependsOnOmitted

instance Eq IndirectlyDependsOnOmitted

instance BG.Generic IndirectlyDependsOnOmitted

instance Show IndirectlyDependsOnOmitted

instance Marshal.StaticSize IndirectlyDependsOnOmitted

instance Marshal.ReadRaw IndirectlyDependsOnOmitted

instance Marshal.WriteRaw IndirectlyDependsOnOmitted

instance BG.Storable IndirectlyDependsOnOmitted

instance Struct.IsStruct IndirectlyDependsOnOmitted
