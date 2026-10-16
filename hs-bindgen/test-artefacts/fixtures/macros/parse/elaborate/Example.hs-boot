{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Outer_int

instance Eq Outer_int

instance BG.Generic Outer_int

instance Ord Outer_int

instance Read Outer_int

instance Show Outer_int

instance BG.Bitfield Outer_int

instance BG.Bits Outer_int

instance Bounded Outer_int

instance Enum Outer_int

instance BG.FiniteBits Outer_int

instance BG.HasFFIType Outer_int

instance Integral Outer_int

instance BG.Ix Outer_int

instance Num Outer_int

instance BG.Prim Outer_int

instance Marshal.ReadRaw Outer_int

instance Real Outer_int

instance Marshal.StaticSize Outer_int

instance BG.Storable Outer_int

instance Marshal.WriteRaw Outer_int

data Inner_int

instance Eq Inner_int

instance BG.Generic Inner_int

instance Ord Inner_int

instance Read Inner_int

instance Show Inner_int

instance BG.Bitfield Inner_int

instance BG.Bits Inner_int

instance Bounded Inner_int

instance Enum Inner_int

instance BG.FiniteBits Inner_int

instance BG.HasFFIType Inner_int

instance Integral Inner_int

instance BG.Ix Inner_int

instance Num Inner_int

instance BG.Prim Inner_int

instance Marshal.ReadRaw Inner_int

instance Real Inner_int

instance Marshal.StaticSize Inner_int

instance BG.Storable Inner_int

instance Marshal.WriteRaw Inner_int
