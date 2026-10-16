{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Foo_8

instance Eq Foo_8

instance BG.Generic Foo_8

instance Show Foo_8

instance Marshal.StaticSize Foo_8

instance Marshal.ReadRaw Foo_8

instance Marshal.WriteRaw Foo_8

instance BG.Storable Foo_8

instance Struct.IsStruct Foo_8

data Foo_16

instance Eq Foo_16

instance BG.Generic Foo_16

instance Show Foo_16

instance Marshal.StaticSize Foo_16

instance Marshal.ReadRaw Foo_16

instance Marshal.WriteRaw Foo_16

instance BG.Storable Foo_16

instance Struct.IsStruct Foo_16

data Foo_32

instance Eq Foo_32

instance BG.Generic Foo_32

instance Show Foo_32

instance Marshal.StaticSize Foo_32

instance Marshal.ReadRaw Foo_32

instance Marshal.WriteRaw Foo_32

instance BG.Storable Foo_32

instance Struct.IsStruct Foo_32

data Foo_64

instance Eq Foo_64

instance BG.Generic Foo_64

instance Show Foo_64

instance Marshal.StaticSize Foo_64

instance Marshal.ReadRaw Foo_64

instance Marshal.WriteRaw Foo_64

instance BG.Storable Foo_64

instance Struct.IsStruct Foo_64

data Bar_8_8

instance Eq Bar_8_8

instance BG.Generic Bar_8_8

instance Show Bar_8_8

instance Marshal.StaticSize Bar_8_8

instance Marshal.ReadRaw Bar_8_8

instance Marshal.WriteRaw Bar_8_8

instance BG.Storable Bar_8_8

instance Struct.IsStruct Bar_8_8

data Bar_8_16

instance Eq Bar_8_16

instance BG.Generic Bar_8_16

instance Show Bar_8_16

instance Marshal.StaticSize Bar_8_16

instance Marshal.ReadRaw Bar_8_16

instance Marshal.WriteRaw Bar_8_16

instance BG.Storable Bar_8_16

instance Struct.IsStruct Bar_8_16

data Bar_8_32

instance Eq Bar_8_32

instance BG.Generic Bar_8_32

instance Show Bar_8_32

instance Marshal.StaticSize Bar_8_32

instance Marshal.ReadRaw Bar_8_32

instance Marshal.WriteRaw Bar_8_32

instance BG.Storable Bar_8_32

instance Struct.IsStruct Bar_8_32

data Bar_8_64

instance Eq Bar_8_64

instance BG.Generic Bar_8_64

instance Show Bar_8_64

instance Marshal.StaticSize Bar_8_64

instance Marshal.ReadRaw Bar_8_64

instance Marshal.WriteRaw Bar_8_64

instance BG.Storable Bar_8_64

instance Struct.IsStruct Bar_8_64

data Bar_16_16

instance Eq Bar_16_16

instance BG.Generic Bar_16_16

instance Show Bar_16_16

instance Marshal.StaticSize Bar_16_16

instance Marshal.ReadRaw Bar_16_16

instance Marshal.WriteRaw Bar_16_16

instance BG.Storable Bar_16_16

instance Struct.IsStruct Bar_16_16

data Bar_16_32

instance Eq Bar_16_32

instance BG.Generic Bar_16_32

instance Show Bar_16_32

instance Marshal.StaticSize Bar_16_32

instance Marshal.ReadRaw Bar_16_32

instance Marshal.WriteRaw Bar_16_32

instance BG.Storable Bar_16_32

instance Struct.IsStruct Bar_16_32

data Bar_16_64

instance Eq Bar_16_64

instance BG.Generic Bar_16_64

instance Show Bar_16_64

instance Marshal.StaticSize Bar_16_64

instance Marshal.ReadRaw Bar_16_64

instance Marshal.WriteRaw Bar_16_64

instance BG.Storable Bar_16_64

instance Struct.IsStruct Bar_16_64

data Bar_32_32

instance Eq Bar_32_32

instance BG.Generic Bar_32_32

instance Show Bar_32_32

instance Marshal.StaticSize Bar_32_32

instance Marshal.ReadRaw Bar_32_32

instance Marshal.WriteRaw Bar_32_32

instance BG.Storable Bar_32_32

instance Struct.IsStruct Bar_32_32

data Bar_32_64

instance Eq Bar_32_64

instance BG.Generic Bar_32_64

instance Show Bar_32_64

instance Marshal.StaticSize Bar_32_64

instance Marshal.ReadRaw Bar_32_64

instance Marshal.WriteRaw Bar_32_64

instance BG.Storable Bar_32_64

instance Struct.IsStruct Bar_32_64

data Bar_64_64

instance Eq Bar_64_64

instance BG.Generic Bar_64_64

instance Show Bar_64_64

instance Marshal.StaticSize Bar_64_64

instance Marshal.ReadRaw Bar_64_64

instance Marshal.WriteRaw Bar_64_64

instance BG.Storable Bar_64_64

instance Struct.IsStruct Bar_64_64
