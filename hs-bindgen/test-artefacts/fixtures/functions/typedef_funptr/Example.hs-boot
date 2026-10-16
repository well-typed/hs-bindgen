{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data RunDriver_Aux

instance BG.Generic RunDriver_Aux

instance BG.ToFunPtr RunDriver_Aux

instance BG.FromFunPtr RunDriver_Aux

data RunDriver

instance Eq RunDriver

instance BG.Generic RunDriver

instance Ord RunDriver

instance Show RunDriver

instance BG.HasFFIType RunDriver

instance Marshal.ReadRaw RunDriver

instance Marshal.StaticSize RunDriver

instance BG.Storable RunDriver

instance Marshal.WriteRaw RunDriver

data Driver

instance Eq Driver

instance BG.Generic Driver

instance Show Driver

instance Marshal.StaticSize Driver

instance Marshal.ReadRaw Driver

instance Marshal.WriteRaw Driver

instance BG.Storable Driver

instance Struct.IsStruct Driver

data Bare

instance Eq Bare

instance BG.Generic Bare

instance Show Bare

instance Marshal.StaticSize Bare

instance Marshal.ReadRaw Bare

instance Marshal.WriteRaw Bare

instance BG.Storable Bare

instance Struct.IsStruct Bare
