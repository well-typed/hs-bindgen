{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union

data Foo_8

instance BG.Generic Foo_8

instance Marshal.StaticSize Foo_8

instance Marshal.ReadRaw Foo_8

instance Marshal.WriteRaw Foo_8

instance BG.Storable Foo_8

instance Union.IsUnion Foo_8

data Foo_16

instance BG.Generic Foo_16

instance Marshal.StaticSize Foo_16

instance Marshal.ReadRaw Foo_16

instance Marshal.WriteRaw Foo_16

instance BG.Storable Foo_16

instance Union.IsUnion Foo_16

data Foo_32

instance BG.Generic Foo_32

instance Marshal.StaticSize Foo_32

instance Marshal.ReadRaw Foo_32

instance Marshal.WriteRaw Foo_32

instance BG.Storable Foo_32

instance Union.IsUnion Foo_32

data Foo_64

instance BG.Generic Foo_64

instance Marshal.StaticSize Foo_64

instance Marshal.ReadRaw Foo_64

instance Marshal.WriteRaw Foo_64

instance BG.Storable Foo_64

instance Union.IsUnion Foo_64

data Foo_8_packed

instance BG.Generic Foo_8_packed

instance Marshal.StaticSize Foo_8_packed

instance Marshal.ReadRaw Foo_8_packed

instance Marshal.WriteRaw Foo_8_packed

instance BG.Storable Foo_8_packed

instance Union.IsUnion Foo_8_packed

data Foo_16_packed

instance BG.Generic Foo_16_packed

instance Marshal.StaticSize Foo_16_packed

instance Marshal.ReadRaw Foo_16_packed

instance Marshal.WriteRaw Foo_16_packed

instance BG.Storable Foo_16_packed

instance Union.IsUnion Foo_16_packed

data Foo_32_packed

instance BG.Generic Foo_32_packed

instance Marshal.StaticSize Foo_32_packed

instance Marshal.ReadRaw Foo_32_packed

instance Marshal.WriteRaw Foo_32_packed

instance BG.Storable Foo_32_packed

instance Union.IsUnion Foo_32_packed

data Foo_64_packed

instance BG.Generic Foo_64_packed

instance Marshal.StaticSize Foo_64_packed

instance Marshal.ReadRaw Foo_64_packed

instance Marshal.WriteRaw Foo_64_packed

instance BG.Storable Foo_64_packed

instance Union.IsUnion Foo_64_packed
