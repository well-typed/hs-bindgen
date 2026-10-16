{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data T1_x

instance Eq T1_x

instance BG.Generic T1_x

instance Show T1_x

instance Marshal.StaticSize T1_x

instance Marshal.ReadRaw T1_x

instance Marshal.WriteRaw T1_x

instance BG.Storable T1_x

instance Struct.IsStruct T1_x

data T1

instance Eq T1

instance BG.Generic T1

instance Show T1

instance Marshal.StaticSize T1

instance Marshal.ReadRaw T1

instance Marshal.WriteRaw T1

instance BG.Storable T1

instance Struct.IsStruct T1

data T2

instance Eq T2

instance BG.Generic T2

instance Show T2

instance Marshal.StaticSize T2

instance Marshal.ReadRaw T2

instance Marshal.WriteRaw T2

instance BG.Storable T2

instance Struct.IsStruct T2

data T2_x

instance Eq T2_x

instance BG.Generic T2_x

instance Show T2_x

instance Marshal.StaticSize T2_x

instance Marshal.ReadRaw T2_x

instance Marshal.WriteRaw T2_x

instance BG.Storable T2_x

instance Struct.IsStruct T2_x

data T3

instance Eq T3

instance BG.Generic T3

instance Show T3

instance Marshal.StaticSize T3

instance Marshal.ReadRaw T3

instance Marshal.WriteRaw T3

instance BG.Storable T3

instance Struct.IsStruct T3

data T3_x

instance Eq T3_x

instance BG.Generic T3_x

instance Show T3_x

instance Marshal.StaticSize T3_x

instance Marshal.ReadRaw T3_x

instance Marshal.WriteRaw T3_x

instance BG.Storable T3_x

instance Struct.IsStruct T3_x
