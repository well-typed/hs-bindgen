{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data S_y_anon'x

instance Eq S_y_anon'x

instance BG.Generic S_y_anon'x

instance Show S_y_anon'x

instance Marshal.StaticSize S_y_anon'x

instance Marshal.ReadRaw S_y_anon'x

instance Marshal.WriteRaw S_y_anon'x

instance BG.Storable S_y_anon'x

instance Struct.IsStruct S_y_anon'x

data S_y

instance Eq S_y

instance BG.Generic S_y

instance Show S_y

instance Marshal.StaticSize S_y

instance Marshal.ReadRaw S_y

instance Marshal.WriteRaw S_y

instance BG.Storable S_y

instance Struct.IsStruct S_y

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S
