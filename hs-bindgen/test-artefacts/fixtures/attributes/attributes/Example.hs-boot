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

data Baz

instance Eq Baz

instance BG.Generic Baz

instance Show Baz

instance Marshal.StaticSize Baz

instance Marshal.ReadRaw Baz

instance Marshal.WriteRaw Baz

instance BG.Storable Baz

instance Struct.IsStruct Baz

data Qux

instance Eq Qux

instance BG.Generic Qux

instance Show Qux

instance Marshal.StaticSize Qux

instance Marshal.ReadRaw Qux

instance Marshal.WriteRaw Qux

instance BG.Storable Qux

instance Struct.IsStruct Qux

data FILE

instance Eq FILE

instance BG.Generic FILE

instance Show FILE

instance Marshal.StaticSize FILE

instance Marshal.ReadRaw FILE

instance Marshal.WriteRaw FILE

instance BG.Storable FILE

instance Struct.IsStruct FILE
