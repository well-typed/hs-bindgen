{-# LANGUAGE NoImplicitPrelude #-}

module M.Foo where

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
