{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S

data T

instance Eq T

instance BG.Generic T

instance Show T

instance Marshal.ReadRaw T

instance Marshal.StaticSize T

instance BG.Storable T

instance Marshal.WriteRaw T

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Show Foo

instance Marshal.ReadRaw Foo

instance Marshal.StaticSize Foo

instance BG.Storable Foo

instance Marshal.WriteRaw Foo

data Bar

instance Eq Bar

instance BG.Generic Bar

instance Ord Bar

instance Show Bar

instance BG.HasFFIType Bar

instance Marshal.ReadRaw Bar

instance Marshal.StaticSize Bar

instance BG.Storable Bar

instance Marshal.WriteRaw Bar
