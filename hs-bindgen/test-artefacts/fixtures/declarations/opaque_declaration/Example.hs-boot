{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Foo

data Bar

instance Eq Bar

instance BG.Generic Bar

instance Show Bar

instance Marshal.StaticSize Bar

instance Marshal.ReadRaw Bar

instance Marshal.WriteRaw Bar

instance BG.Storable Bar

instance Struct.IsStruct Bar

data Baz

instance Eq Baz

instance BG.Generic Baz

instance Show Baz

instance Marshal.StaticSize Baz

instance Marshal.ReadRaw Baz

instance Marshal.WriteRaw Baz

instance BG.Storable Baz

instance Struct.IsStruct Baz

data Quu

data Opaque_union
