{-# LANGUAGE NoImplicitPrelude #-}

module M.Alias where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Window_t

instance Eq Window_t

instance BG.Generic Window_t

instance Show Window_t

instance Marshal.StaticSize Window_t

instance Marshal.ReadRaw Window_t

instance Marshal.WriteRaw Window_t

instance BG.Storable Window_t

instance Struct.IsStruct Window_t
