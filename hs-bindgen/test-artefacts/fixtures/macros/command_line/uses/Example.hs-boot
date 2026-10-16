{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data T

instance Eq T

instance BG.Generic T

instance Ord T

instance Read T

instance Show T

instance BG.Bitfield T

instance BG.Bits T

instance Bounded T

instance Enum T

instance BG.FiniteBits T

instance BG.HasFFIType T

instance Integral T

instance BG.Ix T

instance Num T

instance BG.Prim T

instance Marshal.ReadRaw T

instance Real T

instance Marshal.StaticSize T

instance BG.Storable T

instance Marshal.WriteRaw T

data S

instance Eq S

instance BG.Generic S

instance Show S

instance Marshal.StaticSize S

instance Marshal.ReadRaw S

instance Marshal.WriteRaw S

instance BG.Storable S

instance Struct.IsStruct S
