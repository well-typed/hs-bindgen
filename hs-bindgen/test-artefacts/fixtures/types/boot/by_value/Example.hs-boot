{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data Colour

instance Eq Colour

instance BG.Generic Colour

instance Ord Colour

instance BG.HasFFIType Colour

instance Marshal.StaticSize Colour

instance Marshal.ReadRaw Colour

instance Marshal.WriteRaw Colour

instance BG.Storable Colour

instance BG.Prim Colour

instance CEnum.CEnum Colour

instance CEnum.SequentialCEnum Colour

instance Show Colour

instance Read Colour

data Value

instance BG.Generic Value

instance Marshal.StaticSize Value

instance Marshal.ReadRaw Value

instance Marshal.WriteRaw Value

instance BG.Storable Value

instance Union.IsUnion Value

data Pair

instance Eq Pair

instance BG.Generic Pair

instance Show Pair

instance Marshal.StaticSize Pair

instance Marshal.ReadRaw Pair

instance Marshal.WriteRaw Pair

instance BG.Storable Pair

instance Struct.IsStruct Pair

data Sample

instance Eq Sample

instance BG.Generic Sample

instance Ord Sample

instance Read Sample

instance Show Sample

instance BG.Bitfield Sample

instance BG.Bits Sample

instance Bounded Sample

instance Enum Sample

instance BG.FiniteBits Sample

instance BG.HasFFIType Sample

instance Integral Sample

instance BG.Ix Sample

instance Num Sample

instance BG.Prim Sample

instance Marshal.ReadRaw Sample

instance Real Sample

instance Marshal.StaticSize Sample

instance BG.Storable Sample

instance Marshal.WriteRaw Sample
