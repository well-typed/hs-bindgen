{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data G1

instance Eq G1

instance BG.Generic G1

instance Show G1

instance Marshal.StaticSize G1

instance Marshal.ReadRaw G1

instance Marshal.WriteRaw G1

instance BG.Storable G1

instance Struct.IsStruct G1

data G2

instance Eq G2

instance BG.Generic G2

instance Show G2

instance Marshal.StaticSize G2

instance Marshal.ReadRaw G2

instance Marshal.WriteRaw G2

instance BG.Storable G2

instance Struct.IsStruct G2

data G3

instance Eq G3

instance BG.Generic G3

instance Show G3

instance Marshal.StaticSize G3

instance Marshal.ReadRaw G3

instance Marshal.WriteRaw G3

instance BG.Storable G3

instance Struct.IsStruct G3
