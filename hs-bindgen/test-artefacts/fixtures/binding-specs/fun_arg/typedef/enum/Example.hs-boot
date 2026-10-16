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

data A

instance Eq A

instance BG.Generic A

instance Ord A

instance Read A

instance Show A

instance BG.HasFFIType A

instance BG.Prim A

instance Marshal.ReadRaw A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A

data B

instance Eq B

instance BG.Generic B

instance Ord B

instance Read B

instance Show B

instance BG.HasFFIType B

instance BG.Prim B

instance Marshal.ReadRaw B

instance Marshal.StaticSize B

instance BG.Storable B

instance Marshal.WriteRaw B

data E

instance BG.Generic E

instance BG.HasFFIType E
