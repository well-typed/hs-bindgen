{-# LANGUAGE NoImplicitPrelude #-}

module M.A where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Ord Foo

instance Show Foo

instance BG.HasFFIType Foo

instance Marshal.ReadRaw Foo

instance Marshal.StaticSize Foo

instance BG.Storable Foo

instance Marshal.WriteRaw Foo

data Bar

instance Eq Bar

instance BG.Generic Bar

instance Show Bar

instance Marshal.StaticSize Bar

instance Marshal.ReadRaw Bar

instance Marshal.WriteRaw Bar

instance BG.Storable Bar

instance Struct.IsStruct Bar
