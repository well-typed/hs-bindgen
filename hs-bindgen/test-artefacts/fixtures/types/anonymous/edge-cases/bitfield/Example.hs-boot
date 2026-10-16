{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data S1_anon'y

instance Eq S1_anon'y

instance BG.Generic S1_anon'y

instance Show S1_anon'y

instance Marshal.StaticSize S1_anon'y

instance Marshal.ReadRaw S1_anon'y

instance Marshal.WriteRaw S1_anon'y

instance BG.Storable S1_anon'y

instance Struct.IsStruct S1_anon'y

data S1

instance Eq S1

instance BG.Generic S1

instance Show S1

instance Marshal.StaticSize S1

instance Marshal.ReadRaw S1

instance Marshal.WriteRaw S1

instance BG.Storable S1

instance Struct.IsStruct S1

data S2_anon'anon'y_anon'y

instance Eq S2_anon'anon'y_anon'y

instance BG.Generic S2_anon'anon'y_anon'y

instance Show S2_anon'anon'y_anon'y

instance Marshal.StaticSize S2_anon'anon'y_anon'y

instance Marshal.ReadRaw S2_anon'anon'y_anon'y

instance Marshal.WriteRaw S2_anon'anon'y_anon'y

instance BG.Storable S2_anon'anon'y_anon'y

instance Struct.IsStruct S2_anon'anon'y_anon'y

data S2_anon'anon'y

instance Eq S2_anon'anon'y

instance BG.Generic S2_anon'anon'y

instance Show S2_anon'anon'y

instance Marshal.StaticSize S2_anon'anon'y

instance Marshal.ReadRaw S2_anon'anon'y

instance Marshal.WriteRaw S2_anon'anon'y

instance BG.Storable S2_anon'anon'y

instance Struct.IsStruct S2_anon'anon'y

data S2

instance Eq S2

instance BG.Generic S2

instance Show S2

instance Marshal.StaticSize S2

instance Marshal.ReadRaw S2

instance Marshal.WriteRaw S2

instance BG.Storable S2

instance Struct.IsStruct S2
