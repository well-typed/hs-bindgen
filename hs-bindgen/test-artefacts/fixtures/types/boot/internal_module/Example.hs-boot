{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data On_event_Aux

instance BG.Generic On_event_Aux

instance BG.ToFunPtr On_event_Aux

instance BG.FromFunPtr On_event_Aux

data On_event

instance Eq On_event

instance BG.Generic On_event

instance Ord On_event

instance Show On_event

instance BG.HasFFIType On_event

instance Marshal.ReadRaw On_event

instance Marshal.StaticSize On_event

instance BG.Storable On_event

instance Marshal.WriteRaw On_event

data Buffer_Aux

instance Eq Buffer_Aux

instance BG.Generic Buffer_Aux

instance Show Buffer_Aux

instance Marshal.StaticSize Buffer_Aux

instance Marshal.ReadRaw Buffer_Aux

instance Marshal.WriteRaw Buffer_Aux

instance BG.Storable Buffer_Aux

instance Struct.IsStruct Buffer_Aux

type Buffer = FLAM.WithFlam BG.CChar Buffer_Aux

data Point

instance Eq Point

instance BG.Generic Point

instance Show Point

instance Marshal.StaticSize Point

instance Marshal.ReadRaw Point

instance Marshal.WriteRaw Point

instance BG.Storable Point

instance Struct.IsStruct Point

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

data COUNT_T

instance Eq COUNT_T

instance BG.Generic COUNT_T

instance Ord COUNT_T

instance Read COUNT_T

instance Show COUNT_T

instance BG.Bitfield COUNT_T

instance BG.Bits COUNT_T

instance Bounded COUNT_T

instance Enum COUNT_T

instance BG.FiniteBits COUNT_T

instance BG.HasFFIType COUNT_T

instance Integral COUNT_T

instance BG.Ix COUNT_T

instance Num COUNT_T

instance BG.Prim COUNT_T

instance Marshal.ReadRaw COUNT_T

instance Real COUNT_T

instance Marshal.StaticSize COUNT_T

instance BG.Storable COUNT_T

instance Marshal.WriteRaw COUNT_T

data Handle
