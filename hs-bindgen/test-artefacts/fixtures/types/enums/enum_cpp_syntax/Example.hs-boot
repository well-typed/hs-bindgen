{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data Foo_enum

instance Eq Foo_enum

instance BG.Generic Foo_enum

instance Ord Foo_enum

instance BG.HasFFIType Foo_enum

instance Marshal.StaticSize Foo_enum

instance Marshal.ReadRaw Foo_enum

instance Marshal.WriteRaw Foo_enum

instance BG.Storable Foo_enum

instance BG.Prim Foo_enum

instance CEnum.CEnum Foo_enum

instance CEnum.SequentialCEnum Foo_enum

instance Show Foo_enum

instance Read Foo_enum
