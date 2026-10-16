{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data MyArray

instance Eq MyArray

instance BG.Generic MyArray

instance Show MyArray

instance IsA.IsArray MyArray

instance Marshal.ReadRaw MyArray

instance Marshal.StaticSize MyArray

instance BG.Storable MyArray

instance Marshal.WriteRaw MyArray

data A

instance Eq A

instance BG.Generic A

instance Show A

instance IsA.IsArray A

instance Marshal.ReadRaw A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A

data B

instance Eq B

instance BG.Generic B

instance Show B

instance IsA.IsArray B

instance Marshal.ReadRaw B

instance Marshal.StaticSize B

instance BG.Storable B

instance Marshal.WriteRaw B

data E

instance BG.Generic E
