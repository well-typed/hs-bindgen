{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Primitive

instance Eq Primitive

instance BG.Generic Primitive

instance Show Primitive

instance Marshal.StaticSize Primitive

instance Marshal.ReadRaw Primitive

instance Marshal.WriteRaw Primitive

instance BG.Storable Primitive

instance Struct.IsStruct Primitive
