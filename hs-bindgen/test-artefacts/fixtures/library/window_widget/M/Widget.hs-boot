{-# LANGUAGE NoImplicitPrelude #-}

module M.Widget where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Widget

instance Eq Widget

instance BG.Generic Widget

instance Show Widget

instance Marshal.StaticSize Widget

instance Marshal.ReadRaw Widget

instance Marshal.WriteRaw Widget

instance BG.Storable Widget

instance Struct.IsStruct Widget
