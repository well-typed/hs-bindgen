{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Geo.Point where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Point

instance Eq Point

instance BG.Generic Point

instance Show Point

instance Marshal.StaticSize Point

instance Marshal.ReadRaw Point

instance Marshal.WriteRaw Point

instance BG.Storable Point

instance Struct.IsStruct Point
