{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union

data T

instance BG.Generic T

instance Marshal.StaticSize T

instance Marshal.ReadRaw T

instance Marshal.WriteRaw T

instance BG.Storable T

instance Union.IsUnion T

data S_anon'anon'x_anon'x

instance BG.Generic S_anon'anon'x_anon'x

instance Marshal.StaticSize S_anon'anon'x_anon'x

instance Marshal.ReadRaw S_anon'anon'x_anon'x

instance Marshal.WriteRaw S_anon'anon'x_anon'x

instance BG.Storable S_anon'anon'x_anon'x

instance Struct.IsStruct S_anon'anon'x_anon'x

data S_anon'anon'x

instance BG.Generic S_anon'anon'x

instance Marshal.StaticSize S_anon'anon'x

instance Marshal.ReadRaw S_anon'anon'x

instance Marshal.WriteRaw S_anon'anon'x

instance BG.Storable S_anon'anon'x

instance Struct.IsStruct S_anon'anon'x

data S

instance BG.Generic S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S
