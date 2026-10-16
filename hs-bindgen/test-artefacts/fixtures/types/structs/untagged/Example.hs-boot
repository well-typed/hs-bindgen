{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data S1_c

instance Eq S1_c

instance BG.Generic S1_c

instance Show S1_c

instance Marshal.StaticSize S1_c

instance Marshal.ReadRaw S1_c

instance Marshal.WriteRaw S1_c

instance BG.Storable S1_c

instance Struct.IsStruct S1_c

data S1

instance Eq S1

instance BG.Generic S1

instance Show S1

instance Marshal.StaticSize S1

instance Marshal.ReadRaw S1

instance Marshal.WriteRaw S1

instance BG.Storable S1

instance Struct.IsStruct S1

data S2_inner_deep

instance Eq S2_inner_deep

instance BG.Generic S2_inner_deep

instance Show S2_inner_deep

instance Marshal.StaticSize S2_inner_deep

instance Marshal.ReadRaw S2_inner_deep

instance Marshal.WriteRaw S2_inner_deep

instance BG.Storable S2_inner_deep

instance Struct.IsStruct S2_inner_deep

data S2_inner

instance Eq S2_inner

instance BG.Generic S2_inner

instance Show S2_inner

instance Marshal.StaticSize S2_inner

instance Marshal.ReadRaw S2_inner

instance Marshal.WriteRaw S2_inner

instance BG.Storable S2_inner

instance Struct.IsStruct S2_inner

data S2

instance Eq S2

instance BG.Generic S2

instance Show S2

instance Marshal.StaticSize S2

instance Marshal.ReadRaw S2

instance Marshal.WriteRaw S2

instance BG.Storable S2

instance Struct.IsStruct S2

data S3

instance Eq S3

instance BG.Generic S3

instance Show S3

instance Marshal.StaticSize S3

instance Marshal.ReadRaw S3

instance Marshal.WriteRaw S3

instance BG.Storable S3

instance Struct.IsStruct S3

data S3_c

instance Eq S3_c

instance BG.Generic S3_c

instance Show S3_c

instance Marshal.StaticSize S3_c

instance Marshal.ReadRaw S3_c

instance Marshal.WriteRaw S3_c

instance BG.Storable S3_c

instance Struct.IsStruct S3_c
