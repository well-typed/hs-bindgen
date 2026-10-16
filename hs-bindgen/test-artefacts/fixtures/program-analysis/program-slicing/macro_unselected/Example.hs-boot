{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data U

instance Eq U

instance BG.Generic U

instance Ord U

instance Read U

instance Show U

instance BG.Bitfield U

instance BG.Bits U

instance Bounded U

instance Enum U

instance BG.FiniteBits U

instance BG.HasFFIType U

instance Integral U

instance BG.Ix U

instance Num U

instance BG.Prim U

instance Marshal.ReadRaw U

instance Real U

instance Marshal.StaticSize U

instance BG.Storable U

instance Marshal.WriteRaw U
