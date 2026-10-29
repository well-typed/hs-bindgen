{-# LANGUAGE NoImplicitPrelude #-}

module M.Point where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Status

instance Eq Status

instance BG.Generic Status

instance Ord Status

instance Read Status

instance Show Status

instance BG.Bitfield Status

instance BG.Bits Status

instance Bounded Status

instance Enum Status

instance BG.FiniteBits Status

instance BG.HasFFIType Status

instance Integral Status

instance BG.Ix Status

instance Num Status

instance BG.Prim Status

instance Marshal.ReadRaw Status

instance Real Status

instance Marshal.StaticSize Status

instance BG.Storable Status

instance Marshal.WriteRaw Status
