{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BootFiles.A where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

a_VALUE :: BG.CInt

a_FUNCTION :: forall a0. C.Expr.HostPlatform.Mult a0 BG.CInt => a0 -> C.Expr.HostPlatform.MultRes a0 BG.CInt

data A_enum

instance Eq A_enum

instance BG.Generic A_enum

instance Ord A_enum

instance BG.HasFFIType A_enum

instance Marshal.StaticSize A_enum

instance Marshal.ReadRaw A_enum

instance Marshal.WriteRaw A_enum

instance BG.Storable A_enum

instance BG.Prim A_enum

instance CEnum.CEnum A_enum

instance CEnum.SequentialCEnum A_enum

instance Show A_enum

instance Read A_enum

data A

instance Eq A

instance BG.Generic A

instance Show A

instance Marshal.StaticSize A

instance Marshal.ReadRaw A

instance Marshal.WriteRaw A

instance BG.Storable A

instance Struct.IsStruct A
