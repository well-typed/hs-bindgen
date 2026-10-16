{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data FunPtr_Aux

instance BG.Generic FunPtr_Aux

data FunPtr

instance Eq FunPtr

instance BG.Generic FunPtr

instance Ord FunPtr

instance Show FunPtr

instance BG.HasFFIType FunPtr

instance Marshal.ReadRaw FunPtr

instance Marshal.StaticSize FunPtr

instance BG.Storable FunPtr

instance Marshal.WriteRaw FunPtr

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Show Foo

instance Marshal.StaticSize Foo

instance Marshal.ReadRaw Foo

instance Marshal.WriteRaw Foo

instance BG.Storable Foo

instance Struct.IsStruct Foo
