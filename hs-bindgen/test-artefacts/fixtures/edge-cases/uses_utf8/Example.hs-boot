{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data MyEnum

instance Eq MyEnum

instance BG.Generic MyEnum

instance Ord MyEnum

instance BG.HasFFIType MyEnum

instance Marshal.StaticSize MyEnum

instance Marshal.ReadRaw MyEnum

instance Marshal.WriteRaw MyEnum

instance BG.Storable MyEnum

instance BG.Prim MyEnum

instance CEnum.CEnum MyEnum

instance CEnum.SequentialCEnum MyEnum

instance Show MyEnum

instance Read MyEnum
