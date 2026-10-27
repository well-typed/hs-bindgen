{-# LANGUAGE NoImplicitPrelude #-}

module M.Alias where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Window

instance Eq Window

instance BG.Generic Window

instance Show Window

instance Marshal.StaticSize Window

instance Marshal.ReadRaw Window

instance Marshal.WriteRaw Window

instance BG.Storable Window

instance Struct.IsStruct Window
