{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Floating, Fractional, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

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

data C

instance Eq C

instance BG.Generic C

instance Ord C

instance Read C

instance Show C

instance BG.Bitfield C

instance BG.Bits C

instance Bounded C

instance Enum C

instance BG.FiniteBits C

instance BG.HasFFIType C

instance Integral C

instance BG.Ix C

instance Num C

instance BG.Prim C

instance Marshal.ReadRaw C

instance Real C

instance Marshal.StaticSize C

instance BG.Storable C

instance Marshal.WriteRaw C

data F

instance Eq F

instance BG.Generic F

instance Ord F

instance Read F

instance Show F

instance Enum F

instance Floating F

instance Fractional F

instance BG.HasFFIType F

instance Num F

instance BG.Prim F

instance Marshal.ReadRaw F

instance Real F

instance RealFloat F

instance RealFrac F

instance Marshal.StaticSize F

instance BG.Storable F

instance Marshal.WriteRaw F

data L

instance Eq L

instance BG.Generic L

instance Ord L

instance Read L

instance Show L

instance BG.Bitfield L

instance BG.Bits L

instance Bounded L

instance Enum L

instance BG.FiniteBits L

instance BG.HasFFIType L

instance Integral L

instance BG.Ix L

instance Num L

instance BG.Prim L

instance Marshal.ReadRaw L

instance Real L

instance Marshal.StaticSize L

instance BG.Storable L

instance Marshal.WriteRaw L

data S

instance Eq S

instance BG.Generic S

instance Ord S

instance Read S

instance Show S

instance BG.Bitfield S

instance BG.Bits S

instance Bounded S

instance Enum S

instance BG.FiniteBits S

instance BG.HasFFIType S

instance Integral S

instance BG.Ix S

instance Num S

instance BG.Prim S

instance Marshal.ReadRaw S

instance Real S

instance Marshal.StaticSize S

instance BG.Storable S

instance Marshal.WriteRaw S
