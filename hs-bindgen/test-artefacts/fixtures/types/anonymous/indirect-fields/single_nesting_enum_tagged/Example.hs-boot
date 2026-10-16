{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data T

instance Eq T

instance BG.Generic T

instance Ord T

instance BG.HasFFIType T

instance Marshal.StaticSize T

instance Marshal.ReadRaw T

instance Marshal.WriteRaw T

instance BG.Storable T

instance BG.Prim T

instance CEnum.CEnum T

instance CEnum.SequentialCEnum T

instance Show T

instance Read T

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
