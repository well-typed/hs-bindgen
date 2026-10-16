{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Show Foo

instance Marshal.StaticSize Foo

instance Marshal.ReadRaw Foo

instance Marshal.WriteRaw Foo

instance BG.Storable Foo

instance Struct.IsStruct Foo

data Bar

instance Eq Bar

instance BG.Generic Bar

instance Show Bar

instance Marshal.StaticSize Bar

instance Marshal.ReadRaw Bar

instance Marshal.WriteRaw Bar

instance BG.Storable Bar

instance Struct.IsStruct Bar
