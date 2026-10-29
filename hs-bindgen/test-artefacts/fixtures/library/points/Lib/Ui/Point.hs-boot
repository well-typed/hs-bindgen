{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Ui.Point where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Enum, Eq, Floating, Fractional, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

data Point

instance Eq Point

instance BG.Generic Point

instance Ord Point

instance Read Point

instance Show Point

instance Enum Point

instance Floating Point

instance Fractional Point

instance BG.HasFFIType Point

instance Num Point

instance BG.Prim Point

instance Marshal.ReadRaw Point

instance Real Point

instance RealFloat Point

instance RealFrac Point

instance Marshal.StaticSize Point

instance BG.Storable Point

instance Marshal.WriteRaw Point
