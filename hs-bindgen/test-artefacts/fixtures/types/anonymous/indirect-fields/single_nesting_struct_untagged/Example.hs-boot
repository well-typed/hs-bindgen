{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data S_anon'x_x

instance Eq S_anon'x_x

instance BG.Generic S_anon'x_x

instance Show S_anon'x_x

instance Marshal.StaticSize S_anon'x_x

instance Marshal.ReadRaw S_anon'x_x

instance Marshal.WriteRaw S_anon'x_x

instance BG.Storable S_anon'x_x

instance Struct.IsStruct S_anon'x_x

data S_anon'x

instance Eq S_anon'x

instance BG.Generic S_anon'x

instance Show S_anon'x

instance Marshal.StaticSize S_anon'x

instance Marshal.ReadRaw S_anon'x

instance Marshal.WriteRaw S_anon'x

instance BG.Storable S_anon'x

instance Struct.IsStruct S_anon'x

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S
