{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data I

instance Eq I

instance BG.Generic I

instance Ord I

instance Read I

instance Show I

instance BG.Bitfield I

instance BG.Bits I

instance Bounded I

instance Enum I

instance BG.FiniteBits I

instance BG.HasFFIType I

instance Integral I

instance BG.Ix I

instance Num I

instance BG.Prim I

instance Marshal.ReadRaw I

instance Real I

instance Marshal.StaticSize I

instance BG.Storable I

instance Marshal.WriteRaw I

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S

data U

instance BG.Generic U

instance Marshal.StaticSize U

instance Marshal.ReadRaw U

instance Marshal.WriteRaw U

instance BG.Storable U

instance Union.IsUnion U

data E

instance Eq E

instance BG.Generic E

instance Ord E

instance BG.HasFFIType E

instance Marshal.StaticSize E

instance Marshal.ReadRaw E

instance Marshal.WriteRaw E

instance BG.Storable E

instance BG.Prim E

instance CEnum.CEnum E

instance CEnum.SequentialCEnum E

instance Show E

instance Read E

data TI

instance Eq TI

instance BG.Generic TI

instance Ord TI

instance Read TI

instance Show TI

instance BG.Bitfield TI

instance BG.Bits TI

instance Bounded TI

instance Enum TI

instance BG.FiniteBits TI

instance BG.HasFFIType TI

instance Integral TI

instance BG.Ix TI

instance Num TI

instance BG.Prim TI

instance Marshal.ReadRaw TI

instance Real TI

instance Marshal.StaticSize TI

instance BG.Storable TI

instance Marshal.WriteRaw TI

data TS

instance Eq TS

instance BG.Generic TS

instance Show TS

instance Marshal.ReadRaw TS

instance Marshal.StaticSize TS

instance BG.Storable TS

instance Marshal.WriteRaw TS

data TU

instance BG.Generic TU

instance Marshal.ReadRaw TU

instance Marshal.StaticSize TU

instance BG.Storable TU

instance Marshal.WriteRaw TU

data TE

instance Eq TE

instance BG.Generic TE

instance Ord TE

instance Read TE

instance Show TE

instance BG.HasFFIType TE

instance BG.Prim TE

instance Marshal.ReadRaw TE

instance Marshal.StaticSize TE

instance BG.Storable TE

instance Marshal.WriteRaw TE

data TTI

instance Eq TTI

instance BG.Generic TTI

instance Ord TTI

instance Read TTI

instance Show TTI

instance BG.Bitfield TTI

instance BG.Bits TTI

instance Bounded TTI

instance Enum TTI

instance BG.FiniteBits TTI

instance BG.HasFFIType TTI

instance Integral TTI

instance BG.Ix TTI

instance Num TTI

instance BG.Prim TTI

instance Marshal.ReadRaw TTI

instance Real TTI

instance Marshal.StaticSize TTI

instance BG.Storable TTI

instance Marshal.WriteRaw TTI

data TTS

instance Eq TTS

instance BG.Generic TTS

instance Show TTS

instance Marshal.ReadRaw TTS

instance Marshal.StaticSize TTS

instance BG.Storable TTS

instance Marshal.WriteRaw TTS

data TTU

instance BG.Generic TTU

instance Marshal.ReadRaw TTU

instance Marshal.StaticSize TTU

instance BG.Storable TTU

instance Marshal.WriteRaw TTU

data TTE

instance Eq TTE

instance BG.Generic TTE

instance Ord TTE

instance Read TTE

instance Show TTE

instance BG.HasFFIType TTE

instance BG.Prim TTE

instance Marshal.ReadRaw TTE

instance Marshal.StaticSize TTE

instance BG.Storable TTE

instance Marshal.WriteRaw TTE
