{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data MyStruct

instance Eq MyStruct

instance BG.Generic MyStruct

instance Show MyStruct

instance Marshal.StaticSize MyStruct

instance Marshal.ReadRaw MyStruct

instance Marshal.WriteRaw MyStruct

instance BG.Storable MyStruct

instance Struct.IsStruct MyStruct

data A

instance Eq A

instance BG.Generic A

instance Show A

instance Marshal.ReadRaw A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A

data B

instance Eq B

instance BG.Generic B

instance Show B

instance Marshal.ReadRaw B

instance Marshal.StaticSize B

instance BG.Storable B

instance Marshal.WriteRaw B

data E

instance BG.Generic E
