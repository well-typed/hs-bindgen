{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data BOOL

instance Eq BOOL

instance BG.Generic BOOL

instance Ord BOOL

instance Read BOOL

instance Show BOOL

instance BG.Bitfield BOOL

instance BG.Bits BOOL

instance Bounded BOOL

instance Enum BOOL

instance BG.FiniteBits BOOL

instance BG.HasFFIType BOOL

instance Integral BOOL

instance BG.Ix BOOL

instance Num BOOL

instance BG.Prim BOOL

instance Marshal.ReadRaw BOOL

instance Real BOOL

instance Marshal.StaticSize BOOL

instance BG.Storable BOOL

instance Marshal.WriteRaw BOOL
