{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Ord Foo

instance BG.HasFFIType Foo

instance Marshal.StaticSize Foo

instance Marshal.ReadRaw Foo

instance Marshal.WriteRaw Foo

instance BG.Storable Foo

instance BG.Prim Foo

instance CEnum.CEnum Foo

instance CEnum.SequentialCEnum Foo

instance Show Foo

instance Read Foo
