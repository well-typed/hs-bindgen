{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Baz

instance Eq Baz

instance BG.Generic Baz

instance Show Baz

instance Marshal.StaticSize Baz

instance Marshal.ReadRaw Baz

instance Marshal.WriteRaw Baz

instance BG.Storable Baz

instance Struct.IsStruct Baz
