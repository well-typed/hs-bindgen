{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data TypeA

instance Eq TypeA

instance BG.Generic TypeA

instance Ord TypeA

instance Read TypeA

instance Show TypeA

instance BG.Bitfield TypeA

instance BG.Bits TypeA

instance Bounded TypeA

instance Enum TypeA

instance BG.FiniteBits TypeA

instance BG.HasFFIType TypeA

instance Integral TypeA

instance BG.Ix TypeA

instance Num TypeA

instance BG.Prim TypeA

instance Marshal.ReadRaw TypeA

instance Real TypeA

instance Marshal.StaticSize TypeA

instance BG.Storable TypeA

instance Marshal.WriteRaw TypeA

data TypeB

instance Eq TypeB

instance BG.Generic TypeB

instance Ord TypeB

instance Read TypeB

instance Show TypeB

instance BG.Bitfield TypeB

instance BG.Bits TypeB

instance Bounded TypeB

instance Enum TypeB

instance BG.FiniteBits TypeB

instance BG.HasFFIType TypeB

instance Integral TypeB

instance BG.Ix TypeB

instance Num TypeB

instance BG.Prim TypeB

instance Marshal.ReadRaw TypeB

instance Real TypeB

instance Marshal.StaticSize TypeB

instance BG.Storable TypeB

instance Marshal.WriteRaw TypeB
