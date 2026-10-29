{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Geo.Shape where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Shape

instance Eq Shape

instance BG.Generic Shape

instance Show Shape

instance Marshal.StaticSize Shape

instance Marshal.ReadRaw Shape

instance Marshal.WriteRaw Shape

instance BG.Storable Shape

instance Struct.IsStruct Shape
